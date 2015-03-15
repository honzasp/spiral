#![allow(dead_code)]

#[derive(PartialEq, Debug)]
pub struct ProgDef {
  pub fun_defs: Vec<FunDef>,
  pub string_defs: Vec<StringDef>,
  pub main_fun: FunName,
}

#[derive(PartialEq, Debug)]
pub struct FunDef {
  pub name: FunName,
  pub frame_info: FrameInfo,
  pub blocks: Vec<Block>,
}

#[derive(PartialEq, Debug)]
pub struct FrameInfo {
  pub slot_count: i32,
  pub fun_name_str: StringLabel,
}

#[derive(PartialEq, Debug)]
pub struct StringDef {
  pub label: StringLabel,
  pub bytes: Vec<u8>,
}

#[derive(PartialEq, Debug)]
pub struct Block {
  pub label: Option<Label>,
  pub instrs: Vec<Instr>,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Label(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct StringLabel(pub usize);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct FunName(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct ExternName(pub String);

#[derive(PartialEq, Debug)]
pub enum Instr {
  AddRegImm(Reg, Imm),
  MoveRegImm(Reg, Imm),
  MoveRegReg(Reg, Reg),
  MoveMemImm(Mem, Imm),
  MoveMemReg(Mem, Reg),
  MoveRegMem(Reg, Mem),
  MoveRegAddr(Reg, Mem),
  CallImm(Imm),
  Jump(Imm),
  JumpIf(Test, Imm),
  CmpRegImm(Reg, Imm),
  CmpMemImm(Mem, Imm),
  Return,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Test {
  Overflow,
  NoOverflow,
  Above,
  Below,
  AboveEq,
  BelowEq,
  Equal,
  NotEqual,
  Sign,
  NoSign,
  ParityEven,
  ParityOdd,
  Less,
  Greater,
  LessEq,
  GreaterEq,
}

#[derive(PartialEq, Debug)]
pub enum Reg {
  EAX, EBX, ECX, EDX,
  ESI, EDI, ESP, EBP,
}

#[derive(PartialEq, Debug)]
pub struct Mem {
  pub displac: Option<Imm>,
  pub offset: Option<Reg>,
  pub index: Option<Reg>,
  pub scale: Option<AddrScale>,
}
#[derive(PartialEq, Debug)]
pub enum AddrScale { One, Two, Four }

#[derive(PartialEq, Debug)]
pub enum Imm {
  Int(i32),
  Label(Label),
  FunAddr(FunName),
  FrameInfo(FunName),
  ExternAddr(ExternName),
  StringLabel(StringLabel),
  Plus(Box<Imm>, Box<Imm>),
  Minus(Box<Imm>, Box<Imm>),
  True,
  False,
}
