#![allow(dead_code)]

#[derive(PartialEq, Clone, Debug)]
pub struct ProgDef {
  pub fun_defs: Vec<FunDef>,
  pub obj_defs: Vec<ObjDef>,
  pub main_fun: FunName,
}

#[derive(PartialEq, Clone, Debug)]
pub struct FunDef {
  pub name: FunName,
  pub ret: ContName,
  pub captures: Vec<Var>,
  pub args: Vec<Var>,
  pub body: Term,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ObjDef {
  pub name: ObjName,
  pub obj: Obj,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Obj {
  String(Vec<u8>),
}

#[derive(PartialEq, Clone, Debug)]
pub struct ContDef {
  pub name: ContName,
  pub args: Vec<Var>,
  pub body: Term,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ClosureDef {
  pub var: Var,
  pub fun_name: FunName,
  pub captures: Vec<Val>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Term {
  Letcont(Vec<ContDef>, Box<Term>),
  Letclos(Vec<ClosureDef>, Box<Term>),
  Call(Val, ContName, Vec<Val>),
  ExternCall(ExternName, ContName, Vec<Val>),
  Cont(ContName, Vec<Val>),
  Branch(Boolval, ContName, ContName),
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Var(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct FunName(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct ObjName(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct ExternName(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct ContName(pub String);

#[derive(PartialEq, Debug, Clone)]
pub enum Val {
  Var(Var),
  Combinator(FunName),
  Obj(ObjName),
  Int(i32),
  True,
  False,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Boolval {
  IsTrue(Val),
  IsFalse(Val),
}
