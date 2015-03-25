use std::collections::{HashMap};
use std::rc::{Rc};
use spiral;

#[derive(Debug)]
pub struct Env<V>(Rc<Env_<V>>);

#[derive(Debug)]
enum Env_<V> {
  Empty,
  Bind(HashMap<spiral::Var, V>, Env<V>),
}

impl<V> Clone for Env<V> {
  fn clone(&self) -> Self {
    Env(self.0.clone())
  }
}

impl<V> Env<V> {
  pub fn new() -> Env<V> {
    Env(Rc::new(Env_::Empty))
  }

  pub fn bind_vars(&self, binds: Vec<(spiral::Var, V)>) -> Env<V> {
    Env(Rc::new(Env_::Bind(binds.into_iter().collect(), (*self).clone())))
  }

  pub fn lookup_var<'r>(&'r self, var: &spiral::Var) -> Option<&'r V> {
    match *self.0 {
      Env_::Empty => None,
      Env_::Bind(ref map, ref parent) =>
        map.get(var).or_else(|| parent.lookup_var(var)),
    }
  }
}
