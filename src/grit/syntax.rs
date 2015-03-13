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
  Call(Slot, FunName, Vec<Val>),
  ExternCall(Slot, ExternName, Vec<Val>),
  Assign(Vec<(Slot, Val)>),
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
  Int(i32),
  Slot(Slot),
  True,
  False,
}

#[derive(PartialEq, Debug)]
pub enum Boolval {
  IsTrue(Val),
  IsFalse(Val),
}
