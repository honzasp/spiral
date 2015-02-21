#![allow(dead_code)]
use spine;
pub use spine::{ProgDef, FunDef, ContDef};
pub use spine::Term::{Letval, Letcont, Call, ExternCall, Cont, Branch};
pub use spine::Expr::{Binary, Literal, Var};
pub use spine::Binop::{Add, Sub, Mul, Div, Max, Min,
  Bitand, Bitor, Bitxor, Bitandn};
pub use spine::Boolexpr::{Compare};
pub use spine::Cmp::{Ordered, Unordered, Less, LessEq, Eq, NotEq, GreaterEq, Greater};

pub fn var(id: &str) -> spine::Var {
  spine::Var(id.to_string())
}

pub fn cont(id: &str) -> spine::ContName {
  spine::ContName(id.to_string())
}

pub fn fun(id: &str) -> spine::FunName {
  spine::FunName(id.to_string())
}

pub fn add_e(l: spine::Expr, r: spine::Expr) -> spine::Expr {
  spine::Expr::Binary(spine::Binop::Add, box l, box r)
}

pub fn var_e(id: &str) -> spine::Expr {
  spine::Expr::Var(var(id))
}
