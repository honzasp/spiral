use std::collections::{HashSet};
use spine;
use spiral;
use spine::onion::{Onion};
use spiral::to_spine::mods::{load_mods, translate_mods};
use spiral::to_spine::stmts::{translate_stmts};

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
  let global_env = bind_global_env(&mut st, empty_env);

  let loaded_mods = try!(load_mods(prog, mod_loader));
  let (mod_onions, mods_env) = try!(translate_mods(&mut st, global_env, loaded_mods));
  translate_prog(st, &mods_env, mod_onions, prog)
}

type Res<T> = Result<T, String>;
type Env = spiral::env::Env<spine::Val, Vec<(spiral::Var, spine::Val)>>;

struct ProgSt {
  fun_defs: Vec<spine::FunDef>,
  fun_names: HashSet<spine::FunName>,
  cont_names: HashSet<spine::ContName>,
  vars: HashSet<spine::Var>,
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
  let (body_onion, body_res) = try!(translate_stmts(&mut st, env, &prog.stmts[..]));
  let body_term = body_onion.subst_term(
    spine::Term::Cont(halt_cont.clone(), vec![body_res.into_val()]));

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

fn bind_global_env(st: &mut ProgSt, parent: Env) -> Env {
  let extern_wrappers = &[
      ("println", "spiral_std_println", vec!["x"]),

      ("+",  "spiral_std_add", vec!["a", "b"]),
      ("-",  "spiral_std_sub", vec!["a", "b"]),
      ("*",  "spiral_std_mul", vec!["a", "b"]),
      ("/",  "spiral_std_div", vec!["a", "b"]),
      ("<",  "spiral_std_lt",  vec!["a", "b"]),
      ("<=", "spiral_std_le", vec!["a", "b"]),
      ("==", "spiral_std_eq", vec!["a", "b"]),
      ("/=", "spiral_std_ne", vec!["a", "b"]),
      (">",  "spiral_std_gt",  vec!["a", "b"]),
      (">=", "spiral_std_ge", vec!["a", "b"]),

      ("vec-make",   "spiral_std_vec_make", vec!["len"]),
      ("vec-length", "spiral_std_vec_length", vec!["vec"]),
      ("vec-get",    "spiral_std_vec_get", vec!["vec", "idx"]),
      ("vec-set!",   "spiral_std_vec_set", vec!["vec", "idx", "x"]),
    ];

  let consts = &[
      ("true", spine::Val::True),
      ("false", spine::Val::False),
    ];

  parent.bind_vars(extern_wrappers.iter()
      .map(|&(ref name, ref extern_name, ref args)| {
        let fun_name = st.gen_fun_name(name);
        let ext_name = spine::ExternName(extern_name.to_string());
        let ret_cont = spine::ContName("r".to_string());
        let arg_vars: Vec<_> = args.iter().map(|a| spine::Var(a.to_string())).collect();

        st.fun_defs.push(spine::FunDef {
          name: fun_name.clone(),
          ret: ret_cont.clone(),
          captures: vec![],
          args: arg_vars.clone(),
          body: spine::Term::ExternCall(ext_name, ret_cont,
            arg_vars.into_iter().map(|a| spine::Val::Var(a)).collect()),
        });

        (spiral::Var(name.to_string()), spine::Val::Combinator(fun_name))
      }).collect())
    .bind_vars(consts.iter()
      .map(|&(name, ref val)| (spiral::Var(name.to_string()), val.clone()))
      .collect())
}
