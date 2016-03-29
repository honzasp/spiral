use asm;

pub fn simplify_prog(prog: asm::ProgDef) -> asm::ProgDef {
  asm::ProgDef {
    fun_defs: prog.fun_defs.into_iter().map(simplify_fun).collect(),
    .. prog
  }
}

pub fn simplify_fun(fun: asm::FunDef) -> asm::FunDef {
  let mut blocks = fun.blocks;
  remove_noop_jumps(&mut blocks[..]);
  blocks = blocks.into_iter().map(|block| {
    asm::Block { instrs: improve_instrs(block.instrs), .. block }
  }).collect();
  asm::FunDef { blocks: blocks, .. fun }
}

fn remove_noop_jumps(blocks: &mut [asm::Block]) {
  for idx in 1..blocks.len() {
    let noop_jump = match blocks[idx - 1].instrs.last() {
      Some(&asm::Instr::Jump(asm::Imm::Label(ref label))) => 
        Some(label) == blocks[idx].label.as_ref(),
      _ => false,
    };

    if noop_jump {
      blocks[idx - 1].instrs.pop();
    }
  }
}

fn improve_instrs(instrs: Vec<asm::Instr>) -> Vec<asm::Instr> {
  instrs.into_iter().filter_map(|instr| {
    match instr {
      asm::Instr::AddRegImm(_, ref i) if is_zero_imm(i) => None,
      asm::Instr::SubRegImm(_, ref i) if is_zero_imm(i) => None,
      asm::Instr::MoveRegImm(ref r, ref i) if is_zero_imm(&i) =>
        Some(asm::Instr::XorRegReg(r.clone(), r.clone())),
      asm::Instr::MoveRegReg(ref r1, ref r2) if r1 == r2 => None,
      instr => Some(instr),
    }
  }).collect()
}

fn is_zero_imm(imm: &asm::Imm) -> bool {
  match *imm {
    asm::Imm::Int(num) => num == 0,
    _ => false,
  }
}
