use std::collections::{HashMap};
use std::rc::{Rc};
use spiral;

#[derive(Debug)]
pub struct Env<V, F>(Rc<Env_<V, F>>);

#[derive(Debug)]
enum Env_<V, F> {
  Empty,
  BindVars(HashMap<spiral::Var, V>, Env<V, F>),
  BindFuns(HashMap<spiral::FunName, F>, Env<V, F>),
}

impl<V, F> Clone for Env<V, F> {
  fn clone(&self) -> Self {
    Env(self.0.clone())
  }
}

impl<V, F> Env<V, F> {
  pub fn new() -> Env<V, F> {
    Env(Rc::new(Env_::Empty))
  }

  pub fn bind_vars(&self, binds: Vec<(spiral::Var, V)>) -> Env<V, F> {
    Env(Rc::new(Env_::BindVars(binds.into_iter().collect(), (*self).clone())))
  }

  pub fn bind_funs(&self, binds: Vec<(spiral::FunName, F)>) -> Env<V, F> {
    Env(Rc::new(Env_::BindFuns(binds.into_iter().collect(), (*self).clone())))
  }

  pub fn lookup_var<'r>(&'r self, var: &spiral::Var) -> Option<&'r V> {
    match *self.0 {
      Env_::Empty => None,
      Env_::BindVars(ref map, ref parent) =>
        map.get(var).or_else(|| parent.lookup_var(var)),
      Env_::BindFuns(_, ref parent) => parent.lookup_var(var),
    }
  }

  pub fn lookup_fun<'r>(&'r self, name: &spiral::FunName) -> Option<&'r F> {
    match *self.0 {
      Env_::Empty => None,
      Env_::BindFuns(ref map, ref parent) => 
        map.get(name).or_else(|| parent.lookup_fun(name)),
      Env_::BindVars(_, ref parent) => parent.lookup_fun(name),
    }
  }
}
