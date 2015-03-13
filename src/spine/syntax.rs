#![allow(dead_code)]

#[derive(PartialEq, Debug)]
pub struct ProgDef {
  pub fun_defs: Vec<FunDef>,
  pub main_fun: FunName,
}

#[derive(PartialEq, Debug)]
pub struct FunDef {
  pub name: FunName,
  pub ret: ContName,
  pub args: Vec<Var>,
  pub body: Term,
}

#[derive(PartialEq, Debug)]
pub struct ContDef {
  pub name: ContName,
  pub args: Vec<Var>,
  pub body: Term,
}

#[derive(PartialEq, Debug)]
pub enum Term {
  Letcont(Vec<ContDef>, Box<Term>),
  Call(FunName, ContName, Vec<Val>),
  ExternCall(ExternName, ContName, Vec<Val>),
  Cont(ContName, Vec<Val>),
  Branch(Boolval, ContName, ContName),
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Var(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct FunName(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct ExternName(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct ContName(pub String);

#[derive(PartialEq, Debug, Clone)]
pub enum Val {
  Int(i32),
  Var(Var),
  True,
  False,
}

#[derive(PartialEq, Debug)]
pub enum Boolval {
  IsTrue(Val),
  IsFalse(Val),
}
