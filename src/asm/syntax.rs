#[derive(PartialEq, Debug)]
pub struct ProgDef {
  pub fun_defs: Vec<FunDef>,
  pub obj_defs: Vec<ObjDef>,
  pub string_defs: Vec<StringDef>,
  pub main_fun: FunName,
}

#[derive(PartialEq, Debug)]
pub struct FunDef {
  pub name: FunName,
  pub fun_table: FunTable,
  pub known_start: Label,
  pub blocks: Vec<Block>,
}

#[derive(PartialEq, Debug)]
pub struct FunTable {
  pub slot_count: u32,
  pub arg_count: u32,
  pub capture_count: u32,
  pub fun_name: StringName,
}

#[derive(PartialEq, Debug)]
pub struct ObjDef {
  pub name: ObjName,
  pub obj: Obj,
}

#[derive(PartialEq, Debug)]
pub enum Obj {
  Combinator(FunName),
  String(usize, StringName),
  Double(f64),
}

#[derive(PartialEq, Debug)]
pub struct StringDef {
  pub name: StringName,
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
pub struct ObjName(pub usize);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct StringName(pub usize);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct FunName(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct ExternName(pub String);

#[derive(PartialEq, Debug, Clone)]
pub enum Instr {
  AddRegImm(Reg, Imm),
  SubRegImm(Reg, Imm),
  XorRegReg(Reg, Reg),
  MoveRegImm(Reg, Imm),
  MoveRegReg(Reg, Reg),
  MoveMemImm(Mem, Imm),
  MoveMemReg(Mem, Reg),
  MoveRegMem(Reg, Mem),
  MoveRegAddr(Reg, Mem),
  CallImm(Imm),
  CallMem(Mem),
  Jump(Imm),
  JumpMem(Mem),
  JumpIf(Test, Imm),
  JumpMemIf(Test, Mem),
  CmpRegImm(Reg, Imm),
  CmpMemImm(Mem, Imm),
  TestRegImm(Reg, Imm),
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
  Zero,
  NotZero,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Reg {
  EAX, EBX, ECX, EDX,
  ESI, EDI, ESP, EBP,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Mem {
  pub displac: Option<Imm>,
  pub offset: Option<Reg>,
  pub index: Option<Reg>,
  pub scale: Option<AddrScale>,
}
#[derive(PartialEq, Debug, Clone)]
pub enum AddrScale { One, Two, Four }

#[derive(PartialEq, Debug, Clone)]
pub enum Imm {
  Int(i32),
  Label(Label),
  FunAddr(FunName),
  FunKnownStart(FunName),
  ExternAddr(ExternName),
  ObjAddr(ObjName),
  Plus(Box<Imm>, Box<Imm>),
  Minus(Box<Imm>, Box<Imm>),
  True,
  False,
}
