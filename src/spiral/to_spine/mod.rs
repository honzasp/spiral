use std::collections::{HashSet};
use spine;
use spiral;
use spine::onion::{Onion};
use spiral::to_spine::mods::{load_mods, translate_mods};
use spiral::to_spine::stmts::{translate_stmts_tail};

mod decls;
mod exprs;
mod mods;
mod stmts;
mod test;

pub fn spine_from_spiral(prog: &spiral::Prog,
  mod_loader: &mut (FnMut(&spiral::ModName) -> Result<spiral::Mod, String>))
  -> Result<spine::ProgDef, String> 
{
  let mut st = ProgSt {
      fun_defs: Vec::new(),
      fun_names: HashSet::new(),
      cont_names: HashSet::new(),
      vars: HashSet::new(),
    };
  let empty_env = spiral::env::Env::new();
  let loaded_mods = try!(load_mods(prog, mod_loader));
  let (mod_onions, mods_env) = try!(translate_mods(&mut st, empty_env, loaded_mods));
  translate_prog(st, &mods_env, mod_onions, prog)
}

pub type Res<T> = Result<T, String>;
pub type Env = spiral::env::Env<spine::Val, Vec<(spiral::Var, spine::Val)>>;

pub struct ProgSt {
  pub fun_defs: Vec<spine::FunDef>,
  pub fun_names: HashSet<spine::FunName>,
  pub cont_names: HashSet<spine::ContName>,
  pub vars: HashSet<spine::Var>,
}

impl ProgSt {
  fn gen_fun_name(&mut self, base: &str) -> spine::FunName {
    for i in 1.. {
      let fun_name = spine::FunName(format!("{}_{}", base, i));
      if self.fun_names.insert(fun_name.clone()) {
        return fun_name
      }
    }
    unreachable!()
  }

  fn gen_cont_name(&mut self, base: &str) -> spine::ContName {
    for i in 1.. {
      let cont_name = spine::ContName(format!("{}_{}", base, i));
      if self.cont_names.insert(cont_name.clone()) {
        return cont_name
      }
    }
    unreachable!()
  }

  fn gen_var(&mut self, base: &str) -> spine::Var {
    for i in 1.. {
      let var = spine::Var(format!("{}_{}", base, i));
      if self.vars.insert(var.clone()) {
        return var
      }
    }
    unreachable!()
  }
}

fn translate_prog(mut st: ProgSt, env: &Env, mod_onions: Vec<Onion>, prog: &spiral::Prog) 
  -> Result<spine::ProgDef, String>
{
  let halt_cont = st.gen_cont_name("halt");
  let main_name = st.gen_fun_name("main");
  let body_term = try!(translate_stmts_tail(&mut st, env,
    &prog.stmts[..], halt_cont.clone()));

  let main_term = mod_onions.into_iter().rev().fold(body_term,
    |term, mod_onion| mod_onion.subst_term(term));

  st.fun_defs.push(spine::FunDef {
      name: main_name.clone(),
      ret: halt_cont,
      captures: vec![],
      args: vec![],
      body: main_term,
    });

  Ok(spine::ProgDef {
      fun_defs: st.fun_defs,
      main_fun: main_name,
    })
}
