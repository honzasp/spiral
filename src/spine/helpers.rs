#![allow(dead_code)]
use spine;
pub use spine::{ProgDef, FunDef, ContDef, Term, Val, Boolval};
pub use spine::Term::{Letcont, Call, ExternCall, Cont, Branch};
pub use spine::Val::{Literal, Var};
pub use spine::Boolval::{IsTrue, IsFalse};

pub fn var(id: &str) -> spine::Var {
  spine::Var(id.to_string())
}

pub fn cont(id: &str) -> spine::ContName {
  spine::ContName(id.to_string())
}

pub fn fun(id: &str) -> spine::FunName {
  spine::FunName(id.to_string())
}

pub fn ext_name(id: &str) -> spine::ExternName {
  spine::ExternName(id.to_string())
}

pub fn add_call(ret: &str, l: spine::Val, r: spine::Val) -> spine::Term {
  spine::Term::ExternCall(ext_name("spiral_ext_add"), cont(ret), vec![l, r])
}

pub fn var_val(id: &str) -> spine::Val {
  spine::Val::Var(var(id))
}
