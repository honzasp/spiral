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
    FunSt::stack_mem((self.fun_def.slot_count - slot.0 - 1) as i32)
  }

  fn call_arg_mem(&self, arg: i32) -> asm::Mem {
    FunSt::stack_mem(-3 - arg)
  }

  fn frame_info_mem(&self) -> asm::Mem {
    FunSt::stack_mem(self.fun_def.slot_count as i32)
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
    ((2 + self.fun_def.slot_count) * 4) as i32
  }

  fn stack_frame_shift(&self) -> i32 {
    ((1 + self.fun_def.slot_count) * 4) as i32
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

  let frame_shift = st.stack_frame_shift();
  let frame_info_mem = st.frame_info_mem();
  st.blocks.push(asm::Block {
      label: None,
      instrs: vec![
          asm::Instr::AddRegImm(asm::Reg::ESP, asm::Imm::Int(-frame_shift)),
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
        instrs.push(asm::Instr::AddRegImm(asm::Reg::ESP,
          asm::Imm::Int(-st.extern_call_stack_shift(argc))));
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
      let frame_shift = st.stack_frame_shift();
      move_reg_val(st, &mut instrs, asm::Reg::EAX, val);
      instrs.push(asm::Instr::AddRegImm(asm::Reg::ESP, asm::Imm::Int(frame_shift)));
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
    grit::Val::Slot(ref slot) => {
      instrs.push(asm::Instr::MoveRegMem(asm::Reg::EAX, st.slot_mem(slot)));
      instrs.push(asm::Instr::MoveMemReg(mem, asm::Reg::EAX));
    },
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
    grit::Val::Slot(ref slot) =>
      instrs.push(asm::Instr::MoveRegMem(reg, st.slot_mem(slot))),
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
    .filter(|&i| vals[i] != grit::Val::Slot(slots[i].clone()))
    .collect();
  let mut saved_in_reg: Option<grit::Slot> = None;

  while !assigns.is_empty() {
    let required_slots: HashSet<grit::Slot> = assigns.iter().cloned()
      .filter_map(|i| match vals[i] {
        grit::Val::Slot(ref slot) =>
          if Some(slot) != saved_in_reg.as_ref() {
            Some(slot.clone())
          } else {
            None
          },
        grit::Val::Int(_) | grit::Val::True | grit::Val::False =>
          None,
      }).collect();

    let mut satisfied = Vec::new();
    for i in assigns.iter().cloned() {
      let lslot = &slots[i];
      if !required_slots.contains(lslot) {
        match vals[i] {
          grit::Val::Slot(ref rslot) => {
            if Some(rslot) == saved_in_reg.as_ref() {
              instrs.push(asm::Instr::MoveMemReg(st.slot_mem(lslot), asm::Reg::EDX));
            } else {
              instrs.push(asm::Instr::MoveRegMem(asm::Reg::EAX, st.slot_mem(rslot)));
              instrs.push(asm::Instr::MoveMemReg(st.slot_mem(lslot), asm::Reg::EAX));
            }
          },
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
    grit::Val::Slot(ref slot) => {
      instrs.push(asm::Instr::CmpMemImm(st.slot_mem(slot), imm));
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

fn translate_int(unboxed: i32) -> asm::Imm {
  asm::Imm::Int(unboxed << 1)
}
