use std::collections::{hash_map, HashSet, HashMap};
use grit;
use asm;

pub fn asm_from_grit(prog: &grit::ProgDef) -> asm::ProgDef {
  let mut prog_st = ProgSt {
      used_labels: HashSet::new(),
      string_defs: Vec::new(),
    };

  let fun_defs = prog.fun_defs.iter()
    .map(|fun_def| translate_fun_def(&mut prog_st, fun_def))
    .collect();

  asm::ProgDef {
    fun_defs: fun_defs,
    string_defs: prog_st.string_defs,
    main_fun: translate_fun_name(&prog.main_fun),
  }
}

struct ProgSt {
  used_labels: HashSet<asm::Label>,
  string_defs: Vec<asm::StringDef>,
}

impl ProgSt {
  fn gen_label(&mut self, name: &str) -> asm::Label {
    for i in 1.. {
      let label = asm::Label(format!("{}${}", name, i));
      if self.used_labels.insert(label.clone()) {
        return label
      }
    }
    unreachable!()
  }

  fn gen_string(&mut self, bytes: Vec<u8>) -> asm::StringLabel {
    let str_label = asm::StringLabel(self.string_defs.len());
    self.string_defs.push(asm::StringDef {
      label: str_label.clone(),
      bytes: bytes,
    });
    str_label
  }
}

struct FunSt<'d> {
  prog_st: &'d mut ProgSt,
  fun_def: &'d grit::FunDef,
  emitted_labels: HashSet<grit::Label>,
  block_map: HashMap<grit::Label, &'d grit::Block>,
  label_map: HashMap<grit::Label, asm::Label>,
  blocks: Vec<asm::Block>,
  post_blocks: Vec<asm::Block>,
  slot_alloc: grit::slot_alloc::SlotAlloc,
}

impl<'d> FunSt<'d> {
  fn capture_mem(&self, index: i32) -> asm::Mem {
    asm::Mem {
      displac: Some(asm::Imm::Int(index * 4 + 8 - 0b01)),
      offset: Some(asm::Reg::ECX),
      index: None,
      scale: None,
    }
  }

  fn stack_mem(index: i32) -> asm::Mem {
    asm::Mem {
      displac: Some(asm::Imm::Int(index * 4)),
      offset: Some(asm::Reg::ESP),
      index: None,
      scale: None,
    }
  }

  fn slot_mem(&self, slot: &grit::Slot) -> asm::Mem {
    FunSt::stack_mem((self.slot_alloc.slot_count - slot.0) as i32)
  }

  fn var_mem(&self, var: &grit::Var) -> asm::Mem {
    self.slot_mem(&self.var_to_slot(var))
  }

  fn call_arg_mem(&self, arg: i32) -> asm::Mem {
    FunSt::stack_mem(-2 - arg)
  }

  fn closure_mem(&self) -> asm::Mem {
    FunSt::stack_mem(0)
  }

  fn extern_call_stack_shift(&self, argc: i32) -> i32 {
    -4 * (argc + 2)
  }

  fn extern_call_arg_mem(&self, argc: i32, arg: i32) -> asm::Mem {
    FunSt::stack_mem(-argc + arg)
  }

  fn extern_call_sp_mem(&self, argc: i32) -> asm::Mem {
    FunSt::stack_mem(-argc - 1)
  }

  fn extern_call_bg_mem(&self, argc: i32) -> asm::Mem {
    FunSt::stack_mem(-argc - 2)
  }

  fn stack_frame_size(&self) -> i32 {
    ((2 + self.slot_alloc.slot_count) * 4) as i32
  }

  fn stack_frame_shift(&self) -> i32 {
    ((1 + self.slot_alloc.slot_count) * 4) as i32
  }

  fn translate_label(&mut self, label: &grit::Label) -> asm::Label {
    match self.label_map.entry(label.clone()) {
      hash_map::Entry::Occupied(entry) =>
        entry.get().clone(),
      hash_map::Entry::Vacant(entry) => 
        entry.insert(self.prog_st.gen_label(&label.0[..])).clone(),
    }
  }

  fn var_to_slot(&self, var: &grit::Var) -> grit::Slot {
    self.slot_alloc.vars[var.0].clone()
  }
}

fn translate_fun_def(prog_st: &mut ProgSt, fun_def: &grit::FunDef) -> asm::FunDef {
  let slot_alloc = grit::slot_alloc::alloc(fun_def);
  let mut st = FunSt {
      prog_st: prog_st,
      fun_def: fun_def,
      emitted_labels: HashSet::new(),
      block_map: fun_def.blocks.iter()
        .map(|block| (block.label.clone(), block))
        .collect(),
      label_map: HashMap::new(),
      blocks: Vec::new(),
      post_blocks: Vec::new(),
      slot_alloc: slot_alloc,
    };

  let argc_mismatch_label = st.prog_st.gen_label("argc_mismatch");
  let make_frame_label = st.prog_st.gen_label("make_frame");

  let check_args_block = asm::Block {
    label: None,
    instrs: vec![
      asm::Instr::CmpRegImm(asm::Reg::EAX, translate_int(fun_def.arg_count as i32)),
      asm::Instr::JumpIf(asm::Test::NotEqual,
        asm::Imm::Label(argc_mismatch_label.clone())),
    ],
  };

  let argc_mismatch_block = asm::Block {
    label: Some(argc_mismatch_label),
    instrs: vec![
      asm::Instr::MoveRegImm(asm::Reg::EDX, translate_int(fun_def.arg_count as i32)),
      asm::Instr::MoveRegImm(asm::Reg::ECX,
          asm::Imm::FunAddr(translate_fun_name(&fun_def.name))),
      asm::Instr::Jump(asm::Imm::ExternAddr(
          asm::ExternName("spiral_argc_mismatch".to_string()))),
    ],
  };

  let make_frame_block = {
    let frame_shift = st.stack_frame_shift();
    let closure_mem = st.closure_mem();
    asm::Block {
      label: Some(make_frame_label.clone()),
      instrs: vec![
          asm::Instr::AddRegImm(asm::Reg::ESP, asm::Imm::Int(-frame_shift)),
          if fun_def.capture_count == 0 {
            asm::Instr::MoveMemImm(closure_mem, translate_combinator_obj(&fun_def.name))
          } else {
            asm::Instr::MoveMemReg(closure_mem, asm::Reg::ECX)
          },
        ],
    }
  };

  st.blocks.push(check_args_block);
  st.post_blocks.push(argc_mismatch_block);
  st.blocks.push(make_frame_block);
  emit_block(&mut st, &fun_def.start_label);

  let fun_name_str_label = st.prog_st.gen_string(fun_def.name.0.clone().into_bytes());
  asm::FunDef {
    name: translate_fun_name(&fun_def.name),
    fun_table: asm::FunTable {
      slot_count: st.slot_alloc.slot_count as u32,
      arg_count: fun_def.arg_count as u32,
      capture_count: fun_def.capture_count as u32,
      is_combinator: fun_def.capture_count == 0,
      fun_name_str: fun_name_str_label,
    },
    known_start: make_frame_label,
    blocks: {
      let mut blocks = st.blocks;
      blocks.extend(st.post_blocks.into_iter());
      blocks
    }
  }
}

fn emit_block(st: &mut FunSt, label: &grit::Label) {
  if !st.emitted_labels.insert(label.clone()) {
    return
  }
  let block = *st.block_map.get(label).unwrap();

  let mut instrs = Vec::new();
  for op in block.ops.iter() {
    match *op {
      grit::Op::Call(ref dst_var, ref callee, ref args) => {
        for (arg_idx, arg) in args.iter().enumerate() {
          let arg_mem = st.call_arg_mem(arg_idx as i32);
          move_mem_val(st, &mut instrs, arg_mem, arg);
        }

        match *callee {
          grit::Callee::Combinator(ref fun_name) => {
            instrs.push(asm::Instr::CallImm(asm::Imm::FunKnownStart(
                translate_fun_name(fun_name))));
          },
          grit::Callee::KnownClosure(ref fun_name, ref closure_val) => {
            move_reg_val(st, &mut instrs, asm::Reg::ECX, closure_val);
            instrs.push(asm::Instr::CallImm(asm::Imm::FunKnownStart(
                translate_fun_name(fun_name))));
          },
          grit::Callee::Unknown(ref closure_val) => {
            let invalid_fun_label = st.prog_st.gen_label("invalid_fun");
            move_reg_val(st, &mut instrs, asm::Reg::ECX, closure_val);
            instrs.push(asm::Instr::MoveRegReg(asm::Reg::EDX, asm::Reg::ECX));
            instrs.push(asm::Instr::SubRegImm(asm::Reg::EDX, asm::Imm::Int(0b01)));
            instrs.push(asm::Instr::TestRegImm(asm::Reg::EDX, asm::Imm::Int(0b11)));
            instrs.push(asm::Instr::JumpIf(asm::Test::NotZero,
              asm::Imm::Label(invalid_fun_label.clone())));
            instrs.push(asm::Instr::MoveRegImm(asm::Reg::EAX,
              translate_int(args.len() as i32)));
            instrs.push(asm::Instr::CallMem(asm::Mem {
              displac: Some(asm::Imm::Int(-0b01 + 4)),
              offset: Some(asm::Reg::ECX),
              index: None,
              scale: None,
            }));

            let invalid_fun_block = asm::Block {
              label: Some(invalid_fun_label),
              instrs: vec![
                asm::Instr::MoveMemReg(st.extern_call_arg_mem(1, 0), asm::Reg::ECX),
                asm::Instr::MoveMemReg(st.extern_call_sp_mem(1), asm::Reg::ESP),
                asm::Instr::MoveMemReg(st.extern_call_bg_mem(1), asm::Reg::EDI),
                asm::Instr::AddRegImm(asm::Reg::ESP, 
                  asm::Imm::Int(st.extern_call_stack_shift(1))),
                asm::Instr::CallImm(asm::Imm::ExternAddr(
                  asm::ExternName("spiral_invalid_fun".to_string()))),
              ],
            };
            st.post_blocks.push(invalid_fun_block);
          },
        }

        if st.fun_def.capture_count != 0 {
          instrs.push(asm::Instr::MoveRegMem(asm::Reg::ECX, st.closure_mem()));
        }
        instrs.push(asm::Instr::MoveMemReg(st.var_mem(dst_var), asm::Reg::EAX));
      },
      grit::Op::AllocClos(ref closures) => {
        for &(ref var, ref fun_name, ref captures) in closures.iter() {
          if captures.is_empty() {
            instrs.push(asm::Instr::MoveMemImm(st.var_mem(var),
              translate_combinator_obj(fun_name)));
          } else {
            instrs.push(asm::Instr::MoveMemImm(st.extern_call_arg_mem(2, 0),
              asm::Imm::FunAddr(translate_fun_name(fun_name))));
            instrs.push(asm::Instr::MoveMemImm(st.extern_call_arg_mem(2, 1),
              translate_int(captures.len() as i32)));
            instrs.push(asm::Instr::MoveMemReg(st.extern_call_sp_mem(2), asm::Reg::ESP));
            instrs.push(asm::Instr::MoveMemReg(st.extern_call_bg_mem(2), asm::Reg::EDI));
            instrs.push(asm::Instr::AddRegImm(asm::Reg::ESP,
              asm::Imm::Int(st.extern_call_stack_shift(2))));
            instrs.push(asm::Instr::CallImm(asm::Imm::ExternAddr(
              asm::ExternName("spiral_alloc_closure".to_string()))));
            instrs.push(asm::Instr::SubRegImm(asm::Reg::ESP,
              asm::Imm::Int(st.extern_call_stack_shift(2))));
            instrs.push(asm::Instr::MoveMemReg(st.var_mem(var), asm::Reg::EAX));
          }
        }

        for &(ref var, ref fun_name, ref captures) in closures.iter() {
          if !captures.is_empty() {
            instrs.push(asm::Instr::MoveRegMem(asm::Reg::EDX, st.var_mem(var)));
          }

          for (idx, ref val) in captures.iter().enumerate() {
            move_mem_val(st, &mut instrs, asm::Mem {
              displac: Some(asm::Imm::Int(-0b01 + 8 + 4 * idx as i32)),
              offset: Some(asm::Reg::EDX),
              index: None,
              scale: None,
            }, val);
          }
        }
      },
      grit::Op::ExternCall(ref dst_var, ref extern_name, ref args) => {
        let argc = args.len() as i32;
        for (arg_idx, arg) in args.iter().enumerate() {
          let arg_mem = st.extern_call_arg_mem(argc, arg_idx as i32);
          move_mem_val(st, &mut instrs, arg_mem, arg);
        }
        instrs.push(asm::Instr::MoveMemReg(st.extern_call_sp_mem(argc), asm::Reg::ESP));
        instrs.push(asm::Instr::MoveMemReg(st.extern_call_bg_mem(argc), asm::Reg::EDI));
        let addr_imm = asm::Imm::ExternAddr(translate_extern_name(extern_name));
        instrs.push(asm::Instr::AddRegImm(asm::Reg::ESP,
          asm::Imm::Int(st.extern_call_stack_shift(argc))));
        instrs.push(asm::Instr::CallImm(addr_imm));
        instrs.push(asm::Instr::SubRegImm(asm::Reg::ESP,
          asm::Imm::Int(st.extern_call_stack_shift(argc))));
        instrs.push(asm::Instr::MoveMemReg(st.var_mem(dst_var), asm::Reg::EAX));
      },
      grit::Op::Assign(ref vars_vals) => {
        let slots = vars_vals.iter()
          .map(|&(ref var, _)| st.var_to_slot(var)).collect::<Vec<_>>();
        let vals = vars_vals.iter()
          .map(|&(_, ref val)| val.clone()).collect::<Vec<_>>();
        mass_move(st, &mut instrs, &slots[..], &vals[..]);
      },
    }
  }

  match block.jump {
    grit::Jump::Goto(ref label) => {
      instrs.push(asm::Instr::Jump(
        asm::Imm::Label(st.translate_label(label))));
    },
    grit::Jump::Return(ref val) => {
      let frame_shift = st.stack_frame_shift();
      move_reg_val(st, &mut instrs, asm::Reg::EAX, val);
      instrs.push(asm::Instr::AddRegImm(asm::Reg::ESP, asm::Imm::Int(frame_shift)));
      instrs.push(asm::Instr::Return);
    },
    grit::Jump::TailCall(ref _callee, ref args) => {
      let frame_size = st.stack_frame_size();
      mass_move(st, &mut instrs, 
          &range(0, args.len()).map(grit::Slot).collect::<Vec<_>>()[..], &args[..]);
      instrs.push(asm::Instr::AddRegImm(asm::Reg::ESP, asm::Imm::Int(frame_size)));
      panic!("tail call not implemented");
    },
    grit::Jump::Branch(ref boolval, ref then_label, ref else_label) =>
      match *boolval {
        grit::Boolval::IsTrue(ref val) => {
          cmp_val_imm(st, &mut instrs, val, asm::Imm::False);
          instrs.push(asm::Instr::JumpIf(asm::Test::Equal,
            asm::Imm::Label(st.translate_label(else_label))));
          instrs.push(asm::Instr::Jump(
            asm::Imm::Label(st.translate_label(then_label))));
        },
        grit::Boolval::IsFalse(ref val) => {
          cmp_val_imm(st, &mut instrs, val, asm::Imm::False);
          instrs.push(asm::Instr::JumpIf(asm::Test::NotEqual,
            asm::Imm::Label(st.translate_label(else_label))));
          instrs.push(asm::Instr::Jump(
            asm::Imm::Label(st.translate_label(then_label))));
        },
      },
  }

  let block_label = st.translate_label(label);
  st.blocks.push(asm::Block {
    label: Some(block_label),
    instrs: instrs,
  });

  match block.jump {
    grit::Jump::Goto(ref label) =>
      emit_block(st, label),
    grit::Jump::Branch(_, ref then_label, ref else_label) => {
      emit_block(st, then_label);
      emit_block(st, else_label);
    },
    grit::Jump::Return(_) | grit::Jump::TailCall(_, _) => (),
  }
}

fn move_mem_val(st: &mut FunSt, instrs: &mut Vec<asm::Instr>,
  mem: asm::Mem, val: &grit::Val)
{
  match *val {
    grit::Val::Int(num) => 
      instrs.push(asm::Instr::MoveMemImm(mem, translate_int(num))),
    grit::Val::Var(ref var) => {
      instrs.push(asm::Instr::MoveRegMem(asm::Reg::EAX, st.var_mem(var)));
      instrs.push(asm::Instr::MoveMemReg(mem, asm::Reg::EAX));
    },
    grit::Val::Arg(slot_idx) => {
      instrs.push(asm::Instr::MoveRegMem(asm::Reg::EAX,
        st.slot_mem(&grit::Slot(slot_idx))));
      instrs.push(asm::Instr::MoveMemReg(mem, asm::Reg::EAX));
    },
    grit::Val::Capture(capture_idx) => {
      instrs.push(asm::Instr::MoveRegMem(asm::Reg::EAX,
        st.capture_mem(capture_idx as i32)));
      instrs.push(asm::Instr::MoveMemReg(mem, asm::Reg::EAX));
    },
    grit::Val::Combinator(ref fun_name) =>
      instrs.push(asm::Instr::MoveMemImm(mem, translate_combinator_obj(fun_name))),
    grit::Val::True =>
      instrs.push(asm::Instr::MoveMemImm(mem, asm::Imm::True)),
    grit::Val::False =>
      instrs.push(asm::Instr::MoveMemImm(mem, asm::Imm::False)),
  }
}

fn move_reg_val(st: &mut FunSt, instrs: &mut Vec<asm::Instr>,
  reg: asm::Reg, val: &grit::Val)
{
  match *val {
    grit::Val::Int(num) => 
      instrs.push(asm::Instr::MoveRegImm(reg, translate_int(num))),
    grit::Val::Var(ref var) =>
      instrs.push(asm::Instr::MoveRegMem(reg, st.var_mem(var))),
    grit::Val::Arg(arg_idx) =>
      instrs.push(asm::Instr::MoveRegMem(reg, st.slot_mem(&grit::Slot(arg_idx)))),
    grit::Val::Capture(capture_idx) =>
      instrs.push(asm::Instr::MoveRegMem(reg, st.capture_mem(capture_idx as i32))),
    grit::Val::Combinator(ref fun_name) =>
      instrs.push(asm::Instr::MoveRegImm(reg, translate_combinator_obj(fun_name))),
    grit::Val::True =>
      instrs.push(asm::Instr::MoveRegImm(reg, asm::Imm::True)),
    grit::Val::False =>
      instrs.push(asm::Instr::MoveRegImm(reg, asm::Imm::False)),
  }
}

fn mass_move(st: &mut FunSt, instrs: &mut Vec<asm::Instr>,
  slots: &[grit::Slot], vals: &[grit::Val])
{
  let mut assigns: HashSet<usize> = range(0, slots.len())
    .filter(|&i| match vals[i] {
      grit::Val::Var(ref src_var) => st.var_to_slot(src_var) != slots[i],
      grit::Val::Arg(arg_slot) => grit::Slot(arg_slot) != slots[i],
      _ => true,
    }).collect();
  let mut saved_in_reg: Option<grit::Slot> = None;

  while !assigns.is_empty() {
    let required_slots: HashSet<grit::Slot> = assigns.iter().cloned()
      .filter_map(|i| match vals[i] {
        grit::Val::Var(ref var) => {
          let slot = st.var_to_slot(var);
          if Some(slot.clone()) != saved_in_reg {
            Some(slot)
          } else {
            None
          }
        },
        grit::Val::Arg(slot) => Some(grit::Slot(slot)),
        grit::Val::Capture(_) => None,
        grit::Val::Combinator(_) => None,
        grit::Val::Int(_) | grit::Val::True | grit::Val::False =>
          None,
      }).collect();

    let mut satisfied = Vec::new();
    for i in assigns.iter().cloned() {
      let lslot = &slots[i];
      if !required_slots.contains(lslot) {
        match vals[i] {
          grit::Val::Var(ref rvar) => {
            let rslot = st.var_to_slot(rvar);
            if Some(rslot.clone()) == saved_in_reg {
              instrs.push(asm::Instr::MoveMemReg(st.slot_mem(lslot), asm::Reg::EDX));
            } else {
              instrs.push(asm::Instr::MoveRegMem(asm::Reg::EAX, st.slot_mem(&rslot)));
              instrs.push(asm::Instr::MoveMemReg(st.slot_mem(lslot), asm::Reg::EAX));
            }
          },
          grit::Val::Arg(rslot_idx) => {
            let rslot = grit::Slot(rslot_idx);
            if Some(rslot.clone()) == saved_in_reg {
              instrs.push(asm::Instr::MoveMemReg(st.slot_mem(lslot), asm::Reg::EDX));
            } else {
              instrs.push(asm::Instr::MoveRegMem(asm::Reg::EAX, st.slot_mem(&rslot)));
              instrs.push(asm::Instr::MoveMemReg(st.slot_mem(lslot), asm::Reg::EAX));
            }
          },
          grit::Val::Capture(capture_idx) => {
            instrs.push(asm::Instr::MoveRegMem(asm::Reg::EAX,
              st.capture_mem(capture_idx as i32)));
            instrs.push(asm::Instr::MoveMemReg(st.slot_mem(lslot), asm::Reg::EAX));
          },
          grit::Val::Combinator(ref fun_name) =>
            instrs.push(asm::Instr::MoveMemImm(st.slot_mem(lslot),
              translate_combinator_obj(fun_name))),
          grit::Val::Int(num) => 
            instrs.push(asm::Instr::MoveMemImm(st.slot_mem(lslot), translate_int(num))),
          grit::Val::True =>
            instrs.push(asm::Instr::MoveMemImm(st.slot_mem(lslot), asm::Imm::True)),
          grit::Val::False =>
            instrs.push(asm::Instr::MoveMemImm(st.slot_mem(lslot), asm::Imm::False)),
        }
        satisfied.push(i);
      }
    }

    if satisfied.is_empty() {
      assert!(!required_slots.is_empty());
      let save_slot = required_slots.iter().next().unwrap();
      instrs.push(asm::Instr::MoveRegMem(asm::Reg::EDX, st.slot_mem(save_slot)));
      saved_in_reg = Some(save_slot.clone());
    } else {
      for i in satisfied.iter() {
        assigns.remove(i);
      }
    }
  }
}

fn cmp_val_imm(st: &mut FunSt, instrs: &mut Vec<asm::Instr>,
  val: &grit::Val, imm: asm::Imm)
{
  match *val {
    grit::Val::Int(num) => {
      instrs.push(asm::Instr::MoveRegImm(asm::Reg::EAX, translate_int(num)));
      instrs.push(asm::Instr::CmpRegImm(asm::Reg::EAX, imm));
    },
    grit::Val::Var(ref var) => {
      instrs.push(asm::Instr::CmpMemImm(st.var_mem(var), imm));
    },
    grit::Val::Arg(slot) => {
      instrs.push(asm::Instr::CmpMemImm(st.slot_mem(&grit::Slot(slot)), imm));
    },
    grit::Val::Capture(capture_idx) => {
      instrs.push(asm::Instr::CmpMemImm(st.capture_mem(capture_idx as i32), imm));
    },
    grit::Val::Combinator(ref fun_name) => {
      instrs.push(asm::Instr::MoveRegImm(asm::Reg::EAX,
        translate_combinator_obj(fun_name)));
      instrs.push(asm::Instr::CmpRegImm(asm::Reg::EAX, imm));
    },
    grit::Val::True => {
      instrs.push(asm::Instr::MoveRegImm(asm::Reg::EAX, asm::Imm::True));
      instrs.push(asm::Instr::CmpRegImm(asm::Reg::EAX, imm));
    },
    grit::Val::False => {
      instrs.push(asm::Instr::MoveRegImm(asm::Reg::EAX, asm::Imm::False));
      instrs.push(asm::Instr::CmpRegImm(asm::Reg::EAX, imm));
    },
  }
}

fn translate_fun_name(name: &grit::FunName) -> asm::FunName {
  asm::FunName(name.0.clone())
}

fn translate_extern_name(name: &grit::ExternName) -> asm::ExternName {
  asm::ExternName(name.0.clone())
}

fn translate_combinator_obj(fun_name: &grit::FunName) -> asm::Imm {
  asm::Imm::Plus(
    box asm::Imm::CombinatorObj(translate_fun_name(fun_name)),
    box asm::Imm::Int(0b01))
}

fn translate_int(unboxed: i32) -> asm::Imm {
  asm::Imm::Int(unboxed << 1)
}
