use std::collections::{HashMap};
use std::rc::{Rc};
use spiral;

#[derive(Debug)]
pub struct Env<V, M>(Rc<Env_<V, M>>);

#[derive(Debug)]
enum Env_<V, M> {
  Empty,
  BindVars(HashMap<spiral::Var, V>, Env<V, M>),
  BindMod(spiral::ModName, M, Env<V, M>),
}

impl<V, M> Clone for Env<V, M> {
  fn clone(&self) -> Self {
    Env(self.0.clone())
  }
}

impl<V, M> Env<V, M> {
  pub fn new() -> Env<V, M> {
    Env(Rc::new(Env_::Empty))
  }

  pub fn bind_vars(&self, binds: Vec<(spiral::Var, V)>) -> Env<V, M> {
    Env(Rc::new(Env_::BindVars(binds.into_iter().collect(), (*self).clone())))
  }

  pub fn bind_mod(&self, mod_name: spiral::ModName, bind: M) -> Env<V, M> {
    Env(Rc::new(Env_::BindMod(mod_name, bind, (*self).clone())))
  }

  pub fn lookup_var<'r>(&'r self, var: &spiral::Var) -> Option<&'r V> {
    match *self.0 {
      Env_::Empty => None,
      Env_::BindVars(ref map, ref parent) =>
        map.get(var).or_else(|| parent.lookup_var(var)),
      Env_::BindMod(_, _, ref parent) =>
        parent.lookup_var(var),
    }
  }

  pub fn lookup_mod<'r>(&'r self, mod_name: &spiral::ModName) -> Option<&'r M> {
    match *self.0 {
      Env_::Empty => None,
      Env_::BindVars(_, ref parent) =>
        parent.lookup_mod(mod_name),
      Env_::BindMod(ref bound_name, ref bind, ref parent)=>
        if bound_name == mod_name {
          Some(bind)
        } else {
          parent.lookup_mod(mod_name)
        },
    }
  }
}
