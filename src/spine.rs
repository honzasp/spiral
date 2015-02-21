#![allow(dead_code)]

#[derive(PartialEq, Debug)]
pub struct ProgDef {
  pub fun_defs: Vec<FunDef>,
  pub halt_cont: ContName,
  pub body: Term,
}

#[derive(PartialEq, Debug)]
pub struct ExternDef {
  pub name: FunName,
  pub raw_name: String,
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
  Letval(Var, Expr, Box<Term>),
  Letcont(Vec<ContDef>, Box<Term>),
  Call(FunName, ContName, Vec<Expr>),
  ExternCall(ExternName, ContName, Vec<Expr>),
  Cont(ContName, Vec<Expr>),
  Branch(Boolexpr, ContName, ContName),
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Var(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct FunName(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct ExternName(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct ContName(pub String);

#[derive(PartialEq, Debug)]
pub enum Expr {
  Binary(Binop, Box<Expr>, Box<Expr>),
  Literal(f32),
  Var(Var),
}

#[derive(PartialEq, Debug)]
pub enum Binop {
  Add, Sub, Mul, Div,
  Max, Min,
  Bitand, Bitor, Bitxor, Bitandn,
}

#[derive(PartialEq, Debug)]
pub enum Boolexpr {
  Compare(Cmp, Box<Expr>, Box<Expr>),
}

#[derive(PartialEq, Debug)]
pub enum Cmp {
  Ordered, Unordered,
  Less, LessEq, Eq, NotEq, GreaterEq, Greater,
}
