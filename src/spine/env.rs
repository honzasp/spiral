use std::collections::{HashMap};
use std::rc::{Rc};
use spine;

#[derive(Debug)]
pub struct Env<V, F, C>(Rc<Env_<V, F, C>>);

#[derive(Debug)]
enum Env_<V, F, C> {
  Empty,
  BindVars(HashMap<spine::Var, V>, Env<V, F, C>),
  BindFuns(HashMap<spine::FunName, F>, Env<V, F, C>),
  BindConts(HashMap<spine::ContName, C>, Env<V, F, C>),
}

impl<V, F, C> Clone for Env<V, F, C> {
  fn clone(&self) -> Self {
    Env(self.0.clone())
  }
}

impl<V, F, C> Env<V, F, C> {
  pub fn new() -> Env<V, F, C> {
    Env(Rc::new(Env_::Empty))
  }

  pub fn bind_vars(&self, binds: Vec<(spine::Var, V)>) -> Env<V, F, C> {
    Env(Rc::new(Env_::BindVars(binds.into_iter().collect(), self.clone())))
  }

  pub fn bind_funs<'p>(&self, binds: Vec<(spine::FunName, F)>) -> Env<V, F, C> {
    Env(Rc::new(Env_::BindFuns(binds.into_iter().collect(), self.clone())))
  }

  pub fn bind_conts<'p>(&self, binds: Vec<(spine::ContName, C)>) -> Env<V, F, C> {
    Env(Rc::new(Env_::BindConts(binds.into_iter().collect(), self.clone())))
  }

  pub fn lookup_var<'r>(&'r self, var: &spine::Var) -> Option<&'r V> {
    match *self.0 {
      Env_::Empty => None,
      Env_::BindVars(ref binds, ref parent) =>
        binds.get(var).or_else(|| parent.lookup_var(var)),
      Env_::BindFuns(_, ref parent) | Env_::BindConts(_, ref parent) =>
        parent.lookup_var(var),
    }
  }

  pub fn lookup_fun<'r>(&'r self, fun: &spine::FunName) -> Option<&'r F> {
    match *self.0 {
      Env_::Empty => None,
      Env_::BindFuns(ref binds, ref parent) =>
        binds.get(fun).or_else(|| parent.lookup_fun(fun)),
      Env_::BindVars(_, ref parent) | Env_::BindConts(_, ref parent) =>
        parent.lookup_fun(fun),
    }
  }

  pub fn lookup_cont<'r>(&'r self, cont: &spine::ContName) -> Option<&'r C> {
    match *self.0 {
      Env_::Empty => None,
      Env_::BindConts(ref binds, ref parent) =>
        binds.get(cont).or_else(|| parent.lookup_cont(cont)),
      Env_::BindFuns(_, ref parent) | Env_::BindVars(_, ref parent) =>
        parent.lookup_cont(cont),
    }
  }
}
