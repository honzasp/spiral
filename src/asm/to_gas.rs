use asm;

pub fn gas_from_asm(prog: &asm::ProgDef) -> String {
  let mut lines = Vec::new();
  lines.push(format!("  .text"));
  lines.push(format!(""));

  lines.push(format!("  .globl spiral_start_addr"));
  lines.push(format!("  .type spiral_start_addr,@object"));
  lines.push(format!("spiral_start_addr:"));
  lines.push(format!("  .long {}", fun_name_symbol(&prog.main_fun)));
  lines.push(format!("  .size spiral_start_addr, 4"));
  lines.push(format!(""));

  for fun_def in prog.fun_defs.iter() {
    emit_fun_def(&mut lines, fun_def);
  }

  lines.push(format!(""));
  lines.push(format!("  .section .rodata,\"a\",@progbits"));
  for obj_def in prog.obj_defs.iter() {
    emit_obj_def(&mut lines, obj_def);
  }

  lines.push(format!(""));
  lines.push(format!("  .section .rodata,\"aMS\",@progbits"));
  for str_def in prog.string_defs.iter() {
    emit_str_def(&mut lines, str_def);
  }

  lines.connect("\n")
}

fn emit_fun_def(lines: &mut Vec<String>, fun_def: &asm::FunDef) {
  let symbol = fun_name_symbol(&fun_def.name);

  emit_fun_table(lines, fun_def);
  lines.push(format!("  .globl {}", symbol));
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
  lines.push(format!("  .set {},{}",
    fun_known_start_symbol(&fun_def.name), 
    label_symbol(&fun_def.known_start)));
  lines.push(format!(""));
}

fn emit_obj_def(lines: &mut Vec<String>, obj_def: &asm::ObjDef) {
  let symbol = obj_symbol(&obj_def.name);
  lines.push(format!("  .align 4"));
  lines.push(format!("  .type {},@object", symbol));
  lines.push(format!("{}:", symbol));

  match obj_def.obj {
    asm::Obj::Combinator(ref fun_name) => {
      lines.push(format!("  .long _ZN6spiral17combinator_otableE"));
      lines.push(format!("  .long {}", fun_name_symbol(fun_name)));
      lines.push(format!("  .size {},8", symbol));
    },
    asm::Obj::String(length, ref str_name) => {
      lines.push(format!("  .long _ZN6spiral17static_str_otableE"));
      lines.push(format!("  .long {}", length));
      lines.push(format!("  .long {}", str_symbol(str_name)));
      lines.push(format!("  .size {},12", symbol));
    },
    asm::Obj::Double(number) => {
      lines.push(format!("  .long _ZN6spiral13double_otableE"));
      lines.push(format!("  .double {}", number));
      lines.push(format!("  .byte 1"));
      lines.push(format!("  .skip 3"));
      lines.push(format!("  .size {},16", symbol));
    },
  }
}

fn emit_str_def(lines: &mut Vec<String>, str_def: &asm::StringDef) {
  let symbol = str_symbol(&str_def.name);
  lines.push(format!("{}:", symbol));
  lines.push(format!("  .asciz {}", escape_ascii(&str_def.bytes)));
  lines.push(format!("  .size {},{}", symbol, str_def.bytes.len() + 1));
}

fn emit_fun_table(lines: &mut Vec<String>, fun_def: &asm::FunDef) {
  let table = &fun_def.fun_table;
  let symbol = fun_table_symbol(&fun_def.name);
  lines.push(format!("  .align 16, 0x90"));
  lines.push(format!("  .type {},@object", symbol));
  lines.push(format!("{}:", symbol));
  lines.push(format!("  .long {}", table.slot_count));
  lines.push(format!("  .long {}", table.arg_count));
  lines.push(format!("  .long {}", table.capture_count));
  lines.push(format!("  .long {}", str_symbol(&table.fun_name)));
  lines.push(format!("  .skip 16"));
  lines.push(format!("  .size {},32", symbol));
}

fn translate_instr(instr: &asm::Instr) -> String {
  use asm::Instr as I;

  match *instr {
    I::AddRegImm(ref r1, ref i) =>
      format!("  addl  ${}, {}", imm(i), reg(r1)),
    I::SubRegImm(ref r1, ref i) =>
      format!("  subl  ${}, {}", imm(i), reg(r1)),
    I::XorRegReg(ref r1, ref r2) =>
      format!("  xorl {}, {}", reg(r1), reg(r2)),
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
    I::CallMem(ref m) =>
      format!("  calll *{}", mem(m)),
    I::Jump(ref i) =>
      format!("  jmp   {}", imm(i)),
    I::JumpMem(ref m) =>
      format!("  jmp   *{}", mem(m)),
    I::JumpIf(ref test, ref i) => 
      format!("  j{} {}", cc(test), imm(i)),
    I::JumpMemIf(ref test, ref m) => 
      format!("  j{} *{}", cc(test), mem(m)),
    I::CmpRegImm(ref r1, ref i) =>
      format!("  cmpl  ${}, {}", imm(i), reg(r1)),
    I::CmpMemImm(ref m, ref i) =>
      format!("  cmpl  ${}, {}", imm(i), mem(m)),
    I::TestRegImm(ref r1, ref i) =>
      format!("  testl ${}, {}", imm(i), reg(r1)),
    I::Return =>
      format!("  ret"),
  }
}

fn imm(i: &asm::Imm) -> String {
  match *i {
    asm::Imm::Int(num) => format!("{}", num),
    asm::Imm::Label(ref label) => label_symbol(label),
    asm::Imm::FunAddr(ref name) => fun_name_symbol(name),
    asm::Imm::FunKnownStart(ref name) => fun_known_start_symbol(name),
    asm::Imm::ExternAddr(ref ext_name) => extern_name_symbol(ext_name),
    asm::Imm::ObjAddr(ref obj_name) => obj_symbol(obj_name),
    asm::Imm::Plus(ref l, ref r) => format!("({}) + ({})", imm(&**l), imm(&**r)),
    asm::Imm::Minus(ref l, ref r) => format!("({}) - ({})", imm(&**l), imm(&**r)),
    asm::Imm::True => format!("(_ZN6spiral8true_objE + 0b11)"),
    asm::Imm::False => format!("(_ZN6spiral9false_objE + 0b11)"),
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

fn cc(test: &asm::Test) -> String {
  use asm::Test as T;
  (match *test {
    T::Overflow => "o   ",
    T::NoOverflow => "no  ",
    T::Above => "a   ",
    T::Below => "b   ",
    T::AboveEq => "ae  ",
    T::BelowEq => "be  ",
    T::Equal => "e   ",
    T::NotEqual => "ne  ",
    T::Sign => "s   ",
    T::NoSign => "ns  ",
    T::ParityEven => "pe  ",
    T::ParityOdd => "po  ",
    T::Less => "l   ",
    T::Greater => "g   ",
    T::LessEq => "le  ",
    T::GreaterEq => "ge  ",
    T::Zero => "z   ",
    T::NotZero => "nz  ",
  }).to_string()
}

fn fun_name_symbol(name: &asm::FunName) -> String {
  format!("_spr_{}", z_code(&name.0[..]))
}

fn extern_name_symbol(name: &asm::ExternName) -> String {
  name.0.clone()
}

fn fun_known_start_symbol(name: &asm::FunName) -> String {
  format!(".Lks_{}", z_code(&name.0[..]))
}

fn fun_table_symbol(name: &asm::FunName) -> String {
  format!(".Lft_{}", z_code(&name.0[..]))
}

fn label_symbol(label: &asm::Label) -> String {
  format!(".L_{}", z_code(&label.0[..]))
}

fn obj_symbol(obj_name: &asm::ObjName) -> String {
  format!(".Lobj_{}", obj_name.0)
}

fn str_symbol(str_name: &asm::StringName) -> String {
  format!(".Lstr_{}", str_name.0)
}

fn escape_ascii(bytes: &[u8]) -> String {
  use std::char;
  let mut buf = String::new();
  buf.push('"');
  for &byte in bytes.iter() {
    match char::from_u32(byte as u32) {
      _ if byte == b'\\' => buf.push_str("\\\\"),
      _ if byte == b'"' => buf.push_str("\\\""),
      _ if byte == b'\n' => buf.push_str("\\n"),
      _ if byte == b'\t' => buf.push_str("\\t"),
      _ if byte == b'\r' => buf.push_str("\\r"),
      Some(ch) if byte >= 32 && byte <= 126 => buf.push(ch),
      _ => buf.push_str(format!("\\{:03o}", byte).as_slice()),
    }
  }
  buf.push('"');
  buf
}

fn z_code(txt: &str) -> String {
  let mut buf = String::new();
  for ch in txt.chars() {
    match ch {
      '&' => buf.push_str("za"),
      '^' => buf.push_str("zc"),
      '$' => buf.push_str("zd"),
      '=' => buf.push_str("ze"),
      '>' => buf.push_str("zg"),
      '#' => buf.push_str("zh"),
      '.' => buf.push_str("zi"),
      '?' => buf.push_str("zj"),
      '<' => buf.push_str("zl"),
      '-' => buf.push_str("zm"),
      '!' => buf.push_str("zn"),
      ':' => buf.push_str("zo"),
      '+' => buf.push_str("zp"),
      '\'' => buf.push_str("zq"),
      '\\' => buf.push_str("zr"),
      '/' => buf.push_str("zs"),
      '*' => buf.push_str("zt"),
      '_' => buf.push_str("zu"),
      '%' => buf.push_str("zv"),
      '~' => buf.push_str("zw"),
      'z' => buf.push_str("zz"),
      ch if ch >= 'a' && ch <= 'y' => buf.push(ch),
      ch if ch >= 'A' && ch <= 'Z' => buf.push(ch),
      ch if ch >= '0' && ch <= '9' => buf.push(ch),
      ch => buf.push_str(&format!("z{}", ch as u32)[..]),
    }
  }
  buf
}
