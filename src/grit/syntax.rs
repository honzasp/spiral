#![allow(dead_code)]

#[derive(PartialEq, Debug, Clone)]
pub struct ProgDef {
  pub fun_defs: Vec<FunDef>,
  pub main_fun: FunName,
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunDef {
  pub name: FunName,
  pub capture_count: usize,
  pub arg_count: usize,
  pub var_count: usize,
  pub blocks: Vec<Block>,
  pub start_label: Label,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Block {
  pub label: Label,
  pub ops: Vec<Op>,
  pub jump: Jump,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Op {
  Call(Var, Callee, Vec<Val>),
  ExternCall(Var, ExternName, Vec<Val>),
  AllocClos(Vec<(Var, FunName, Vec<Val>)>),
  Assign(Vec<(Var, Val)>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Jump {
  Goto(Label),
  Return(Val),
  TailCall(Callee, Vec<Val>),
  Branch(Boolval, Label, Label),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Callee {
  Combinator(FunName),
  KnownClosure(FunName, Val),
  Unknown(Val),
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Var(pub usize);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Label(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct FunName(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct ExternName(pub String);

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Slot(pub usize);

#[derive(PartialEq, Debug, Clone)]
pub enum Val {
  Var(Var),
  Arg(usize),
  Capture(usize),
  Combinator(FunName),
  Int(i32),
  True,
  False,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Boolval {
  IsTrue(Val),
  IsFalse(Val),
}
