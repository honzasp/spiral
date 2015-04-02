#![allow(dead_code)]

#[derive(PartialEq, Debug)]
pub struct Prog {
  pub stmts: Vec<Stmt>,
}

#[derive(PartialEq, Debug)]
pub struct Mod {
  pub name: ModName,
  pub decls: Vec<Decl>,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Var(pub String);
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct ModName(pub String);

#[derive(PartialEq, Debug)]
pub enum Decl {
  Export(Vec<Var>),
  Stmt(Stmt),
}

#[derive(PartialEq, Debug)]
pub enum Stmt {
  Import(Vec<ImportDef>),
  Fun(FunDef),
  Var(Var, Expr),
  Expr(Expr),
}

#[derive(PartialEq, Debug)]
pub enum ImportDef {
  Mod(ModName),
  Only(Box<ImportDef>, Vec<Var>),
  Except(Box<ImportDef>, Vec<Var>),
  Prefix(Box<ImportDef>, Var),
}

#[derive(PartialEq, Debug)]
pub struct FunDef {
  pub var: Var,
  pub args: Vec<Var>,
  pub stmts: Vec<Stmt>,
}

#[derive(PartialEq, Debug)]
pub enum Expr {
  If(Box<Expr>, Box<Expr>, Box<Expr>),
  Cond(Vec<(Expr, Vec<Stmt>)>),
  When(Box<Expr>, Vec<Stmt>),
  Unless(Box<Expr>, Vec<Stmt>),
  Do(Vec<(Var, Expr, Expr)>, Box<Expr>, Vec<Stmt>, Vec<Stmt>),
  And(Vec<Expr>),
  Or(Vec<Expr>),
  Begin(Vec<Stmt>),
  Let(Vec<(Var, Expr)>, Vec<Stmt>),
  Call(Box<Expr>, Vec<Expr>),
  Lambda(Vec<Var>, Vec<Stmt>),
  Extern(Var, Vec<Expr>),
  Var(Var),
  Int(i32),
  Double(f64),
  String(String),
}
