use spine;

type Env = spine::env::Env<(), usize, usize>;

pub fn check(prog: &spine::ProgDef) -> Vec<String> {
  let fun_binds = prog.fun_defs.iter().map(|fun_def| {
      (fun_def.name.clone(), fun_def.args.len())
    }).collect();
  let empty_env = spine::env::Env::new();
  let fun_env = empty_env.bind_funs(fun_binds);

  let mut errors = Vec::new();
  for fun_def in prog.fun_defs.iter() {
    errors.extend(check_fun_def(&fun_env, fun_def).into_iter());
  }

  if fun_env.lookup_fun(&prog.main_fun).is_none() {
    errors.push(format!("undefined main function: '{}'", prog.main_fun.0));
  }

  errors
}

fn check_fun_def(env: &Env, fun_def: &spine::FunDef) -> Vec<String> {
  let var_binds = fun_def.args.iter().map(|arg| {
      (arg.clone(), ())
    }).collect();
  let cont_binds = vec![
      (fun_def.ret.clone(), 1),
    ];
  let args_env = env.bind_vars(var_binds);
  let cont_env = args_env.bind_conts(cont_binds);
  check_term(&cont_env, &fun_def.body)
}

fn check_cont_def(env: &Env, cont_def: &spine::ContDef) -> Vec<String> {
  let var_binds = cont_def.args.iter().map(|arg| {
      (arg.clone(), ())
    }).collect();
  let args_env = env.bind_vars(var_binds);
  check_term(&args_env, &cont_def.body)
}

fn check_term(env: &Env, term: &spine::Term) -> Vec<String> {
  let mut errors = Vec::new();

  match *term {
    spine::Term::Letcont(ref cont_defs, ref body) => {
      let cont_binds = cont_defs.iter()
          .map(|cont_def| (cont_def.name.clone(), cont_def.args.len()))
          .collect();
      let inner_env = env.bind_conts(cont_binds);
      for cont_def in cont_defs.iter() {
        errors.extend(check_cont_def(&inner_env, cont_def).into_iter());
      }
      errors.extend(check_term(&inner_env, &**body).into_iter());
    },
    spine::Term::Call(ref fun_name, ref ret_cont, ref args) => {
      match env.lookup_fun(fun_name) {
        Some(&argc) => if argc != args.len() {
          errors.push(format!("call args mismatch: '{}' expects {} args, got {}",
            fun_name.0, argc, args.len()))
        },
        None => errors.push(format!("call to undefined fun: '{}'", fun_name.0)),
      }

      match env.lookup_cont(ret_cont) {
        Some(&retc) => if retc != 1 {
          errors.push(format!("return args mismatch: '{}' expects {} args",
            ret_cont.0, retc))
        },
        None => errors.push(format!("return to undefined cont: '{}'", ret_cont.0)),
      }

      for arg in args.iter() {
        errors.extend(check_val(env, arg).into_iter());
      }
    },
    spine::Term::ExternCall(ref _extern_name, ref ret_cont, ref args) => {
      match env.lookup_cont(ret_cont) {
        Some(&retc) => if retc != 1 {
          errors.push(format!("extern return args mismatch: '{}' expects {} args",
            ret_cont.0, retc))
        },
        None => errors.push(format!("extern return to undefined cont: '{}'", ret_cont.0)),
      }

      for arg in args.iter() {
        errors.extend(check_val(env, arg).into_iter());
      }
    },
    spine::Term::Cont(ref cont, ref args) => {
      match env.lookup_cont(cont) {
        Some(&argc) => if argc != args.len() {
          errors.push(format!("continue args mismatch: '{}' expects {} args, got {}",
            cont.0, argc, args.len()));
        },
        None => errors.push(format!("continue to undefined cont: '{}'", cont.0)),
      }

      for arg in args.iter() {
        errors.extend(check_val(env, arg).into_iter());
      }
    },
    spine::Term::Branch(ref boolval, ref then_cont, ref else_cont) => {
      match env.lookup_cont(then_cont) {
        Some(&argc) => if argc != 0 {
          errors.push(format!("then-branch args mismatch: '{}' expects {} args",
            then_cont.0, argc));
        },
        None => errors.push(format!("then-branch to undefined cont: '{}'", then_cont.0)),
      }

      match env.lookup_cont(else_cont) {
        Some(&argc) => if argc != 0 {
          errors.push(format!("else-branch args mismatch: '{}' expects {} args",
            else_cont.0, argc));
        },
        None => errors.push(format!("else-branch to undefined cont: '{}'", else_cont.0)),
      }

      errors.extend(check_boolval(env, boolval).into_iter());
    }
  }

  errors
}

fn check_val(env: &Env, val: &spine::Val) -> Vec<String> {
  match *val {
    spine::Val::Literal(_) => vec![],
    spine::Val::Var(ref var) => match env.lookup_var(var) {
      Some(&()) => vec![],
      None => vec![format!("undefined var: '{}'", var.0)],
    },
  }
}

fn check_boolval(env: &Env, boolval: &spine::Boolval) -> Vec<String> {
  match *boolval {
    spine::Boolval::IsTrue(ref val) => check_val(env, val),
    spine::Boolval::IsFalse(ref val) => check_val(env, val),
  }
}

#[cfg(test)]
mod test {
  use spine::check::{check_prog};
  use spine::helpers::*;

  #[test]
  fn test_halt_cont_ok() {
    assert!(check_prog(&ProgDef {
        fun_defs: vec![],
        halt_cont: cont("heaven"),
        body: Cont(cont("heaven"), vec![]),
      }).is_empty());
  }

  #[test]
  fn test_halt_cont_args_mismatch() {
    let errors = check_prog(&ProgDef {
        fun_defs: vec![],
        halt_cont: cont("emergency"),
        body: Cont(cont("emergency"), vec![Literal(1.0)]),
      });
    assert_eq!(errors.len(), 1);
    assert!(errors[0].contains("continue args mismatch"));
  }

  #[test]
  fn test_undefined_var() {
    let errors = check_prog(&ProgDef {
        fun_defs: vec![
          FunDef { name: fun("f"), ret: cont("ret"), 
            args: vec![var("a"), var("b")],
            body: Cont(cont("ret"), vec![var_val("c")]),
          },
        ],
        halt_cont: cont("halt"),
        body: Cont(cont("halt"), vec![]),
      });
    assert_eq!(errors.len(), 1);
    assert!(errors[0].contains("undefined var: 'c'"));
  }

  // TODO :-)
}
