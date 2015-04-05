#![allow(dead_code)]

#[derive(PartialEq, Clone, Debug)]
pub struct ProgDef {
  pub halt_cont: ContName,
  pub body: Term,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ContDef {
  pub name: ContName,
  pub args: Vec<Var>,
  pub body: Term,
}

#[derive(PartialEq, Clone, Debug)]
pub struct FunDef {
  pub var: Var,
  pub ret: ContName,
  pub captures: Vec<Var>,
  pub args: Vec<Var>,
  pub body: Term,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ObjDef {
  pub var: Var,
  pub obj: Obj,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Obj {
  String(Vec<u8>),
  Double(f64),
}


#[derive(PartialEq, Clone, Debug)]
pub enum Term {
  Letcont(Vec<ContDef>, Box<Term>),
  Letfun(Vec<FunDef>, Box<Term>),
  Letobj(ObjDef, Box<Term>),
  Call(Val, ContName, Vec<Val>),
  ExternCall(ExternName, ContName, Vec<Val>),
  Cont(ContName, Vec<Val>),
  Branch(Boolval, ContName, ContName),
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Var(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct ExternName(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct ContName(pub String);

#[derive(PartialEq, Debug, Clone)]
pub enum Val {
  Var(Var),
  Int(i32),
  True,
  False,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Boolval {
  IsTrue(Val),
  IsFalse(Val),
}
