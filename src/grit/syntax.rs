#![allow(dead_code)]

#[derive(PartialEq, Debug)]
pub struct ProgDef {
  pub fun_defs: Vec<FunDef>,
  pub main_fun: FunName,
}

#[derive(PartialEq, Debug)]
pub struct FunDef {
  pub name: FunName,
  pub arg_count: usize,
  pub slot_count: usize,
  pub blocks: Vec<Block>,
  pub start_label: Label,
}

#[derive(PartialEq, Debug)]
pub struct Block {
  pub label: Label,
  pub ops: Vec<Op>,
  pub jump: Jump,
}

#[derive(PartialEq, Debug)]
pub enum Op {
  Call(LVal, FunName, Vec<Val>),
  ExternCall(LVal, ExternName, Vec<Val>),
  Assign(Vec<(LVal, Val)>),
}

#[derive(PartialEq, Debug)]
pub enum Jump {
  Goto(Label),
  Return(Val),
  TailCall(FunName, Vec<Val>),
  Branch(Boolval, Label, Label),
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Slot(pub usize);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Label(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct FunName(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct ExternName(pub String);

#[derive(PartialEq, Debug, Clone)]
pub enum Val {
  Literal(f32),
  Slot(Slot),
  Reg(Reg),
}

#[derive(PartialEq, Debug, Clone)]
pub enum LVal {
  Literal(f32),
  Slot(Slot),
  Reg(Reg),
}


#[derive(PartialEq, Debug)]
pub enum Boolval {
  IsTrue(Val),
  IsFalse(Val),
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum Reg {
  Reg1, Reg2, Reg3, Reg4,
}
