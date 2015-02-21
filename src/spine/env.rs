use std::collections::{HashMap};
use spine;

#[derive(Debug)]
pub enum Env<'p, V: 'static, F: 'static, C: 'static> {
  Empty,
  BindVars(HashMap<spine::Var, V>, &'p Env<'p, V, F, C>),
  BindFuns(HashMap<spine::FunName, F>, &'p Env<'p, V, F, C>),
  BindConts(HashMap<spine::ContName, C>, &'p Env<'p, V, F, C>),
}

impl<'q, V: Clone, F: Clone, C: Clone> Env<'q, V, F, C> {
  pub fn new() -> Env<'static, V, F, C> {
    Env::Empty
  }

  pub fn bind_vars<'p>(&'p self, binds: Vec<(spine::Var, V)>) -> Env<'p, V, F, C> {
    Env::BindVars(binds.into_iter().collect(), self)
  }

  pub fn bind_funs<'p>(&'p self, binds: Vec<(spine::FunName, F)>) -> Env<'p, V, F, C> {
    Env::BindFuns(binds.into_iter().collect(), self)
  }

  pub fn bind_conts<'p>(&'p self, binds: Vec<(spine::ContName, C)>) -> Env<'p, V, F, C> {
    Env::BindConts(binds.into_iter().collect(), self)
  }

  pub fn lookup_var<'r>(&'r self, var: &spine::Var) -> Option<&'r V> {
    match *self {
      Env::Empty => None,
      Env::BindVars(ref binds, parent) =>
        binds.get(var).or_else(|| parent.lookup_var(var)),
      Env::BindFuns(_, parent) | Env::BindConts(_, parent) =>
        parent.lookup_var(var),
    }
  }

  pub fn lookup_fun<'r>(&'r self, fun: &spine::FunName) -> Option<&'r F> {
    match *self {
      Env::Empty => None,
      Env::BindFuns(ref binds, parent) =>
        binds.get(fun).or_else(|| parent.lookup_fun(fun)),
      Env::BindVars(_, parent) | Env::BindConts(_, parent) =>
        parent.lookup_fun(fun),
    }
  }

  pub fn lookup_cont<'r>(&'r self, cont: &spine::ContName) -> Option<&'r C> {
    match *self {
      Env::Empty => None,
      Env::BindConts(ref binds, parent) =>
        binds.get(cont).or_else(|| parent.lookup_cont(cont)),
      Env::BindFuns(_, parent) | Env::BindVars(_, parent) =>
        parent.lookup_cont(cont),
    }
  }
}
