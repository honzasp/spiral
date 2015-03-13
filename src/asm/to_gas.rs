use asm;

pub fn gas_from_asm(prog: &asm::ProgDef) -> String {
  let mut lines = Vec::new();
  lines.push(format!("  .text"));
  lines.push(format!("  .globl spiral_start"));
  lines.push(format!("  .set spiral_start, {}", fun_name_symbol(&prog.main_fun)));
  lines.push(format!("  .set {}, 0xffffffe", true_symbol()));
  lines.push(format!("  .set {}, 0xfffffff", false_symbol()));
  lines.push(format!(""));

  for fun_def in prog.fun_defs.iter() {
    emit_fun_def(&mut lines, fun_def);
  }
  lines.connect("\n")
}

fn emit_fun_def(lines: &mut Vec<String>, fun_def: &asm::FunDef) {
  let symbol = fun_name_symbol(&fun_def.name);
  lines.push(format!("  .globl {}", symbol));
  lines.push(format!("  .align 16, 0x90"));
  lines.push(format!("  .type  {},@function", symbol));
  lines.push(format!("{}:", symbol));

  for block in fun_def.blocks.iter() {
    match block.label {
      Some(ref label) => 
        lines.push(format!("{}:", label_symbol(label))),
      None => (),
    }

    for instr in block.instrs.iter() {
      lines.push(translate_instr(instr));
    }
  }

  lines.push(format!(".Lend_{}:", symbol));
  lines.push(format!("  .size {0}, .Lend_{0} - {0}", symbol));
  lines.push(format!(""));
}

fn translate_instr(instr: &asm::Instr) -> String {
  use asm::Instr as I;
  use asm::Test as T;

  match *instr {
    I::AddRegImm(ref r1, ref i) =>
      format!("  addl  ${}, {}", imm(i), reg(r1)),
    I::MoveRegImm(ref r1, ref i) =>
      format!("  movl  ${}, {}", imm(i), reg(r1)),
    I::MoveRegReg(ref r1, ref r2) =>
      format!("  movl  {}, {}", reg(r2), reg(r1)),
    I::MoveMemImm(ref m, ref i) =>
      format!("  movl  ${}, {}", imm(i), mem(m)),
    I::MoveMemReg(ref m, ref r1) =>
      format!("  movl  {}, {}", reg(r1), mem(m)),
    I::MoveRegMem(ref r1, ref m) =>
      format!("  movl  {}, {}", mem(m), reg(r1)),
    I::MoveRegAddr(ref r1, ref m) =>
      format!("  leal  {}, {}", mem(m), reg(r1)),
    I::CallImm(ref i) =>
      format!("  calll {}", imm(i)),
    I::Jump(ref i) =>
      format!("  jmp   {}", imm(i)),
    I::JumpIf(ref test, ref i) => 
      format!("  {1} {0}", imm(i), match *test {
        T::Overflow => "jo   ",
        T::NoOverflow => "jno  ",
        T::Above => "ja   ",
        T::Below => "jb   ",
        T::AboveEq => "jae  ",
        T::BelowEq => "jbe  ",
        T::Equal => "je   ",
        T::NotEqual => "jne  ",
        T::Sign => "js   ",
        T::NoSign => "jns  ",
        T::ParityEven => "jpe  ",
        T::ParityOdd => "jpo  ",
        T::Less => "jl   ",
        T::Greater => "jg   ",
        T::LessEq => "jle  ",
        T::GreaterEq => "jge  ",
      }),
    I::CmpRegImm(ref r1, ref i) =>
      format!("  cmpl  ${}, {}", imm(i), reg(r1)),
    I::CmpMemImm(ref m, ref i) =>
      format!("  cmpl  ${}, {}", imm(i), mem(m)),
    I::Return =>
      format!("  ret"),
  }
}

fn imm(i: &asm::Imm) -> String {
  match *i {
    asm::Imm::Int(num) => format!("{}", num),
    asm::Imm::Label(ref label) => label_symbol(label),
    asm::Imm::FunAddr(ref name) => fun_name_symbol(name),
    asm::Imm::ExternAddr(ref ext_name) => extern_name_symbol(ext_name),
    asm::Imm::Plus(ref l, ref r) => format!("({}) + ({})", imm(&**l), imm(&**r)),
    asm::Imm::Minus(ref l, ref r) => format!("({}) - ({})", imm(&**l), imm(&**r)),
    asm::Imm::True => true_symbol(),
    asm::Imm::False => false_symbol(),
  }
}

fn reg(reg: &asm::Reg) -> String {
  match *reg {
    asm::Reg::EAX => format!("%eax"),
    asm::Reg::EBX => format!("%ebx"),
    asm::Reg::ECX => format!("%ecx"),
    asm::Reg::EDX => format!("%edx"),
    asm::Reg::ESP => format!("%esp"),
    asm::Reg::EBP => format!("%ebp"),
    asm::Reg::EDI => format!("%edi"),
    asm::Reg::ESI => format!("%esi"),
  }
}

fn mem(mem: &asm::Mem) -> String {
  format!("{}({}{}{})",
    mem.displac.as_ref().map(imm).unwrap_or(format!("")),
    mem.offset.as_ref().map(reg).unwrap_or(format!("")),
    mem.index.as_ref().map(|r| format!(", {}", reg(r))).unwrap_or(format!("")),
    mem.scale.as_ref().map(|scale| match *scale {
      asm::AddrScale::One => format!(", 1"),
      asm::AddrScale::Two => format!(", 2"),
      asm::AddrScale::Four => format!(", 4"),
    }).unwrap_or(format!("")),
  )
}

fn fun_name_symbol(name: &asm::FunName) -> String {
  format!("_spr_{}", name.0)
}

fn extern_name_symbol(name: &asm::ExternName) -> String {
  name.0.clone()
}

fn label_symbol(label: &asm::Label) -> String {
  format!(".L_{}", label.0)
}

fn true_symbol() -> String {
  format!(".Ltrue")
}

fn false_symbol() -> String {
  format!(".Lfalse")
}
