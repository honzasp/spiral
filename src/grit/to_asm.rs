use std::collections::{hash_map, HashSet, HashMap};
use grit;
use asm;

pub fn asm_from_grit(prog: &grit::ProgDef) -> asm::ProgDef {
  let mut prog_st = ProgSt {
      used_labels: HashSet::new(),
    };

  asm::ProgDef {
    fun_defs: prog.fun_defs.iter()
      .map(|fun_def| translate_fun_def(&mut prog_st, fun_def))
      .collect(),
    main_fun: translate_fun_name(&prog.main_fun),
  }
}

struct ProgSt {
  used_labels: HashSet<asm::Label>,
}

impl ProgSt {
  fn gen_label(&mut self, name: &grit::Label) -> asm::Label {
    for i in 1.. {
      let label = asm::Label(format!("{}${}", name.0, i));
      if self.used_labels.insert(label.clone()) {
        return label
      }
    }
    unreachable!()
  }
}

struct FunSt<'d> {
  prog_st: &'d mut ProgSt,
  fun_def: &'d grit::FunDef,
  emitted_labels: HashSet<grit::Label>,
  block_map: HashMap<grit::Label, &'d grit::Block>,
  label_map: HashMap<grit::Label, asm::Label>,
  blocks: Vec<asm::Block>,
}

impl<'d> FunSt<'d> {
  fn stack_mem(index: i32) -> asm::Mem {
    asm::Mem {
      displac: Some(asm::Imm::Int(index * 4)),
      offset: Some(asm::Reg::ESP),
      index: None,
      scale: None,
    }
  }

  fn slot_mem(&self, slot: &grit::Slot) -> asm::Mem {
    FunSt::stack_mem((self.fun_def.slot_count - slot.0) as i32)
  }

  fn call_arg_mem(&self, arg: i32) -> asm::Mem {
    FunSt::stack_mem(-2 - arg)
  }

  fn frame_info_mem(&self) -> asm::Mem {
    FunSt::stack_mem(self.fun_def.slot_count as i32)
  }

  fn extern_call_arg_mem(&self, argc: i32, arg: i32) -> asm::Mem {
    FunSt::stack_mem(-argc + arg)
  }

  fn extern_call_stack_shift(&self, argc: i32) -> i32 {
    -(argc * 4)
  }

  fn stack_frame_size(&self) -> i32 {
    ((2 + self.fun_def.slot_count) * 4) as i32
  }

  fn translate_label(&mut self, label: &grit::Label) -> asm::Label {
    match self.label_map.entry(label.clone()) {
      hash_map::Entry::Occupied(entry) =>
        entry.get().clone(),
      hash_map::Entry::Vacant(entry) => 
        entry.insert(self.prog_st.gen_label(label)).clone(),
    }
  }
}

fn translate_fun_def(prog_st: &mut ProgSt, fun_def: &grit::FunDef) -> asm::FunDef {
  let mut st = FunSt {
      prog_st: prog_st,
      fun_def: fun_def,
      emitted_labels: HashSet::new(),
      block_map: fun_def.blocks.iter()
        .map(|block| (block.label.clone(), block))
        .collect(),
      label_map: HashMap::new(),
      blocks: Vec::new(),
    };

  let frame_size = st.stack_frame_size();
  let frame_info_mem = st.frame_info_mem();
  st.blocks.push(asm::Block {
      label: None,
      instrs: vec![
          asm::Instr::AddRegImm(asm::Reg::ESP, asm::Imm::Int(-frame_size)),
          asm::Instr::MoveMemImm(frame_info_mem, asm::Imm::Int(123456)),
        ],
    });
  emit_block(&mut st, &fun_def.start_label);

  asm::FunDef {
    name: translate_fun_name(&fun_def.name),
    blocks: st.blocks,
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
      grit::Op::Call(ref dst_slot, ref fun_name, ref args) => {
        for (arg_idx, arg) in args.iter().enumerate() {
          let arg_mem = st.call_arg_mem(arg_idx as i32);
          move_mem_val(st, &mut instrs, arg_mem, arg);
        }
        let addr_imm = asm::Imm::FunAddr(translate_fun_name(fun_name));
        instrs.push(asm::Instr::CallImm(addr_imm));
        instrs.push(asm::Instr::MoveMemReg(st.slot_mem(dst_slot), asm::Reg::EAX));
      },
      grit::Op::ExternCall(ref dst_slot, ref extern_name, ref args) => {
        let argc = args.len();
        for (arg_idx, arg) in args.iter().enumerate() {
          let arg_mem = st.extern_call_arg_mem(argc as i32, arg_idx as i32);
          move_mem_val(st, &mut instrs, arg_mem, arg);
        }
        let addr_imm = asm::Imm::ExternAddr(translate_extern_name(extern_name));
        instrs.push(asm::Instr::AddRegImm(asm::Reg::ESP,
          asm::Imm::Int(st.extern_call_stack_shift(argc as i32))));
        instrs.push(asm::Instr::CallImm(addr_imm));
        instrs.push(asm::Instr::AddRegImm(asm::Reg::ESP,
          asm::Imm::Int(-st.extern_call_stack_shift(argc as i32))));
        instrs.push(asm::Instr::MoveMemReg(st.slot_mem(dst_slot), asm::Reg::EAX));
      },
      grit::Op::Assign(ref slots_vals) =>
        mass_move(st, &mut instrs,
            &slots_vals.iter().map(|&(ref slot, _)| slot.clone()).collect::<Vec<_>>()[..],
            &slots_vals.iter().map(|&(_, ref val)| val.clone()).collect::<Vec<_>>()[..]),
    }
  }

  match block.jump {
    grit::Jump::Goto(ref label) => {
      instrs.push(asm::Instr::Jump(
        asm::Imm::Label(st.translate_label(label))));
    },
    grit::Jump::Return(ref val) => {
      let frame_size = st.stack_frame_size();
      move_reg_val(st, &mut instrs, asm::Reg::EAX, val);
      instrs.push(asm::Instr::AddRegImm(asm::Reg::ESP, asm::Imm::Int(frame_size)));
      instrs.push(asm::Instr::Return);
    },
    grit::Jump::TailCall(ref fun_name, ref args) => {
      let frame_size = st.stack_frame_size();
      mass_move(st, &mut instrs, 
          &range(0, args.len()).map(grit::Slot).collect::<Vec<_>>()[..], &args[..]);
      instrs.push(asm::Instr::AddRegImm(asm::Reg::ESP, asm::Imm::Int(frame_size)));
      instrs.push(asm::Instr::Jump(
        asm::Imm::FunAddr(translate_fun_name(fun_name))));
    },
    grit::Jump::Branch(ref boolval, ref then_label, ref else_label) =>
      match *boolval {
        grit::Boolval::IsTrue(ref val) => {
          cmp_val_imm(st, &mut instrs, val, asm::Imm::Float(0.0));
          instrs.push(asm::Instr::JumpIf(asm::Test::Equal,
            asm::Imm::Label(st.translate_label(else_label))));
          instrs.push(asm::Instr::Jump(
            asm::Imm::Label(st.translate_label(then_label))));
        },
        grit::Boolval::IsFalse(ref val) => {
          cmp_val_imm(st, &mut instrs, val, asm::Imm::Float(0.0));
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
    grit::Val::Literal(num) => {
      instrs.push(asm::Instr::MoveMemImm(mem, asm::Imm::Float(num)));
    },
    grit::Val::Slot(ref slot) => {
      instrs.push(asm::Instr::MoveRegMem(asm::Reg::EDX, st.slot_mem(slot)));
      instrs.push(asm::Instr::MoveMemReg(mem, asm::Reg::EDX));
    },
  }
}

fn move_reg_val(st: &mut FunSt, instrs: &mut Vec<asm::Instr>,
  reg: asm::Reg, val: &grit::Val)
{
  match *val {
    grit::Val::Literal(num) => 
      instrs.push(asm::Instr::MoveRegImm(reg, asm::Imm::Float(num))),
    grit::Val::Slot(ref slot) =>
      instrs.push(asm::Instr::MoveRegMem(reg, st.slot_mem(slot))),
  }
}

fn mass_move(st: &mut FunSt, instrs: &mut Vec<asm::Instr>,
  slots: &[grit::Slot], vals: &[grit::Val])
{
  let mut remaining: HashSet<usize> = range(0, slots.len()).collect();
  let mut saved_in_reg = None;

  while !remaining.is_empty() {
    let required: HashSet<grit::Slot> = remaining.iter().cloned()
      .filter_map(|i| match vals[i] {
        grit::Val::Slot(ref src_slot) =>
          if saved_in_reg != Some(src_slot.clone()) {
            Some(src_slot.clone()) 
          } else {
            None 
          },
        grit::Val::Literal(_) => None,
      }).collect();
    let mut satisfied = Vec::new();

    for &i in remaining.iter() {
      if !required.contains(&slots[i]) {
        match (&saved_in_reg, &vals[i]) {
          (&Some(ref saved_slot), &grit::Val::Slot(ref read_slot)) => {
            assert_eq!(saved_slot, read_slot);
            instrs.push(asm::Instr::MoveMemReg(st.slot_mem(&slots[i]), asm::Reg::EDX));
          },
          (_, &grit::Val::Slot(ref read_slot)) => {
            instrs.push(asm::Instr::MoveRegMem(asm::Reg::EDX, st.slot_mem(read_slot)));
            instrs.push(asm::Instr::MoveMemReg(st.slot_mem(&slots[i]), asm::Reg::EDX));
          },
          (_, &grit::Val::Literal(num)) => {
            instrs.push(asm::Instr::MoveMemImm(st.slot_mem(&slots[i]),
              asm::Imm::Float(num)));
          },
        }
        satisfied.push(i);
      }
    }

    if satisfied.is_empty() {
      let breaker = required.iter().next().unwrap();
      instrs.push(asm::Instr::MoveRegMem(asm::Reg::EDX, st.slot_mem(breaker)));
      saved_in_reg = Some(breaker.clone());
    }

    for i in satisfied.into_iter() {
      remaining.remove(&i);
    }
  }
}

fn cmp_val_imm(st: &mut FunSt, instrs: &mut Vec<asm::Instr>,
  val: &grit::Val, imm: asm::Imm)
{
  match *val {
    grit::Val::Literal(num) => {
      instrs.push(asm::Instr::MoveRegImm(asm::Reg::EDX, asm::Imm::Float(num)));
      instrs.push(asm::Instr::CmpRegImm(asm::Reg::EDX, imm));
    },
    grit::Val::Slot(ref slot) => {
      instrs.push(asm::Instr::CmpMemImm(st.slot_mem(slot), imm));
    },
  }
}

fn translate_fun_name(name: &grit::FunName) -> asm::FunName {
  asm::FunName(name.0.clone())
}

fn translate_extern_name(name: &grit::ExternName) -> asm::ExternName {
  asm::ExternName(name.0.clone())
}