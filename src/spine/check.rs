use spine;
use std::collections::{HashSet};

type Env = spine::env::Env<(), usize, usize>;

pub fn check(prog: &spine::ProgDef) -> Vec<String> {
  let fun_binds = prog.fun_defs.iter().map(|fun_def| {
      (fun_def.name.clone(), fun_def.args.len())
    }).collect();
  let empty_env = spine::env::Env::new();
  let fun_env = empty_env.bind_funs(fun_binds);

  let mut errors = Vec::new();
  let mut fun_names = HashSet::new();
  for fun_def in prog.fun_defs.iter() {
    errors.extend(check_fun_def(&fun_env, fun_def).into_iter());
    if !fun_names.insert(&fun_def.name) {
      errors.push(format!("redefined function: '{}'", fun_def.name.0));
    }
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
  use sexpr;
  use spine;

  fn parse_check(txt: &str) -> Vec<String> {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    let spine = sexpr::to_spine::prog_from_sexpr(&sexpr).unwrap();
    spine::check::check(&spine)
  }

  #[test]
  fn test_missing_main() {
    let errs = parse_check("(program question (answer r () (cont r 42)))");
    assert_eq!(errs.len(), 1);
    assert!(errs[0].contains("undefined main function: 'question'"));
  }

  #[test]
  fn test_double_fun_defs() {
    let errs = parse_check("(program main 
      (main k () (cont k 10))
      (main k () (cont k 20)))");
    assert_eq!(errs.len(), 1);
    assert!(errs[0].contains("redefined function: 'main'"));
  }

  #[test]
  fn test_undefined_var() {
    let errs = parse_check("(program main (main halt () (cont halt x)))");
    assert_eq!(errs.len(), 1);
    assert!(errs[0].contains("undefined var: 'x'"));
  }

  #[test]
  fn test_undefined_conts() {
    let errs = parse_check(
      "(program main 
        (fun r (a b) (cont r 100))
        (main r () 
          (letcont ((bad-call () (call fun cc1 1 2))
                    (bad-ext-call () (extern-call ext_add cc2 1 2))
                    (bad-cont () (cont cc3 3))
                    (bad-branch () (branch (is-true 1) cc4 cc5)))
            (cont r 5))))");

    assert_eq!(errs.len(), 5);
    assert!(errs[0].contains("return to undefined cont: 'cc1'"));
    assert!(errs[1].contains("extern return to undefined cont: 'cc2'"));
    assert!(errs[2].contains("continue to undefined cont: 'cc3'"));
    assert!(errs[3].contains("then-branch to undefined cont: 'cc4'"));
    assert!(errs[4].contains("else-branch to undefined cont: 'cc5'"));
  }

  #[test]
  fn test_cont_args_mismatch() {
    let errs = parse_check(
      "(program main
        (main r () (cont r 0))
        (return-42 r () (cont r 42))
        (bad-return-2 ret (a b) (cont ret a b))
        (bad-return-0 ret (a b) (cont ret))
        (bad-call r () 
          (letcont ((needs-two-args (x y) (cont r x)))
            (call return-42 needs-two-args)))
        (bad-extern-call r ()
          (letcont ((needs-zero-args () (cont r 10)))
            (extern-call __boo__ needs-zero-args 1 2 3)))
        (bad-branch r ()
          (letcont ((needs-one-arg (x) (cont r x))
                    (needs-two-args (x y) (cont r y)))
            (branch (is-true 0) needs-one-arg needs-two-args))))");

    assert_eq!(errs.len(),  6);
    assert!(errs[0].contains("continue args mismatch: 'ret' expects 1 args"));
    assert!(errs[1].contains("continue args mismatch: 'ret' expects 1 args"));
    assert!(errs[2].contains("return args mismatch: 'needs-two-args' expects 2 args"));
    assert!(errs[3].contains("extern return args mismatch: \
      'needs-zero-args' expects 0 args"));
    assert!(errs[4].contains("then-branch args mismatch: \
      'needs-one-arg' expects 1 args"));
    assert!(errs[5].contains("else-branch args mismatch: \
      'needs-two-args' expects 2 args"));
  }
}
