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
  pub cont_names: HashSet<spine::ContName>,
  pub vars: HashSet<spine::Var>,
}

impl ProgSt {
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
  let body_term = try!(translate_stmts_tail(&mut st, env,
      &prog.stmts[..], halt_cont.clone()));
  let prog_term = mod_onions.into_iter().rev().fold(body_term,
    |term, mod_onion| mod_onion.subst_term(term));
  Ok(spine::ProgDef { halt_cont: halt_cont, body: prog_term })
}
