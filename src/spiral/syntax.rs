#![allow(dead_code)]

#[derive(PartialEq, Debug)]
pub struct Prog {
  pub body: Vec<Stmt>,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Var(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct FunName(pub String);

#[derive(PartialEq, Debug)]
pub enum Stmt {
  Fun(FunName, Vec<Var>, Vec<Stmt>),
  Var(Var, Expr),
  Expr(Expr),
}

#[derive(PartialEq, Debug)]
pub enum Expr {
  If(Box<Expr>, Box<Expr>, Box<Expr>),
  Cond(Vec<(Expr, Vec<Stmt>)>, Option<Vec<Stmt>>),
  When(Box<Expr>, Vec<Stmt>),
  Unless(Box<Expr>, Vec<Stmt>),
  Do(Vec<(Var, Expr, Expr)>, Box<Expr>, Vec<Stmt>, Vec<Stmt>),
  And(Vec<Expr>),
  Or(Vec<Expr>),
  Begin(Vec<Stmt>),
  Let(Vec<(Var, Expr)>, Vec<Stmt>),
  Call(FunName, Vec<Expr>),
  Var(Var),
  Int(i32),
}
