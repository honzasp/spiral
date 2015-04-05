use std::collections::{HashMap};
use std::rc::{Rc};
use spine;

#[derive(Debug)]
pub struct Env<V, C>(Rc<Env_<V, C>>);

#[derive(Debug)]
enum Env_<V, C> {
  Empty,
  BindVars(HashMap<spine::Var, V>, Env<V, C>),
  BindConts(HashMap<spine::ContName, C>, Env<V, C>),
}

impl<V, C> Clone for Env<V, C> {
  fn clone(&self) -> Self {
    Env(self.0.clone())
  }
}

impl<V, C> Env<V, C> {
  pub fn new() -> Env<V, C> {
    Env(Rc::new(Env_::Empty))
  }

  pub fn bind_vars(&self, binds: Vec<(spine::Var, V)>) -> Env<V, C> {
    Env(Rc::new(Env_::BindVars(binds.into_iter().collect(), self.clone())))
  }

  pub fn bind_conts<'p>(&self, binds: Vec<(spine::ContName, C)>) -> Env<V, C> {
    Env(Rc::new(Env_::BindConts(binds.into_iter().collect(), self.clone())))
  }

  pub fn lookup_var<'r>(&'r self, var: &spine::Var) -> Option<&'r V> {
    match *self.0 {
      Env_::Empty => None,
      Env_::BindVars(ref binds, ref parent) =>
        binds.get(var).or_else(|| parent.lookup_var(var)),
      Env_::BindConts(_, ref parent) =>
        parent.lookup_var(var),
    }
  }

  pub fn lookup_cont<'r>(&'r self, cont: &spine::ContName) -> Option<&'r C> {
    match *self.0 {
      Env_::Empty => None,
      Env_::BindConts(ref binds, ref parent) =>
        binds.get(cont).or_else(|| parent.lookup_cont(cont)),
      Env_::BindVars(_, ref parent) =>
        parent.lookup_cont(cont),
    }
  }
}
