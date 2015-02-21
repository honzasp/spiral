use std::collections::{HashMap};
use spiral;

#[derive(Debug)]
pub enum Env<'p, V: 'static, F: 'static> {
  Empty,
  BindVars(HashMap<spiral::Var, V>, &'p Env<'p, V, F>),
  BindFuns(HashMap<spiral::FunName, F>, &'p Env<'p, V, F>),
}

impl<'q, V, F> Env<'q, V, F> {
  pub fn new() -> Env<'static, V, F> {
    Env::Empty
  }

  pub fn bind_vars<'p>(&'p self, binds: Vec<(spiral::Var, V)>) -> Env<'p, V, F> {
    Env::BindVars(binds.into_iter().collect(), self)
  }

  pub fn bind_funs<'p>(&'p self, binds: Vec<(spiral::FunName, F)>) -> Env<'p, V, F> {
    Env::BindFuns(binds.into_iter().collect(), self)
  }

  pub fn lookup_var<'r>(&'r self, var: &spiral::Var) -> Option<&'r V> {
    match *self {
      Env::Empty => None,
      Env::BindVars(ref map, parent) =>
        map.get(var).or_else(|| parent.lookup_var(var)),
      Env::BindFuns(_, parent) => parent.lookup_var(var),
    }
  }

  pub fn lookup_fun<'r>(&'r self, name: &spiral::FunName) -> Option<&'r F> {
    match *self {
      Env::Empty => None,
      Env::BindFuns(ref map, parent) => 
        map.get(name).or_else(|| parent.lookup_fun(name)),
      Env::BindVars(_, parent) => parent.lookup_fun(name),
    }
  }
}
