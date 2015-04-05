use spine;

type Env = spine::env::Env<(), usize>;

pub fn check(prog: &spine::ProgDef) -> Vec<String> {
  let env = spine::env::Env::new().bind_conts(vec![(prog.halt_cont.clone(), 1)]);
  check_term(&env, &prog.body)
}

fn check_fun_def(env: &Env, fun_def: &spine::FunDef) -> Vec<String> {
  let mut errors = Vec::new();

  for capture in fun_def.captures.iter() {
    match env.lookup_var(capture) {
      None => errors.push(format!("undefined capture: '{}'", capture.0)),
      Some(_) => (),
    }
  }

  let var_binds = fun_def.args.iter().chain(fun_def.captures.iter())
    .map(|var| (var.clone(), ())).collect();
  let cont_binds = vec![(fun_def.ret.clone(), 1)];
  let inner_env = spine::env::Env::new().bind_vars(var_binds).bind_conts(cont_binds);
  errors.extend(check_term(&inner_env, &fun_def.body).into_iter());

  errors
}

fn check_cont_def(env: &Env, cont_def: &spine::ContDef) -> Vec<String> {
  let var_binds = cont_def.args.iter().map(|arg| (arg.clone(), ())).collect();
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
    spine::Term::Letfun(ref fun_defs, ref body) => {
      let fun_binds = fun_defs.iter().map(|def| (def.var.clone(), ())).collect();
      let inner_env = env.bind_vars(fun_binds);
      for fun_def in fun_defs.iter() {
        errors.extend(check_fun_def(&inner_env, fun_def).into_iter());
      }
      errors.extend(check_term(&inner_env, &**body).into_iter());
    },
    spine::Term::Letobj(ref obj_def, ref body) => {
      let var_binds = vec![(obj_def.var.clone(), ())];
      errors = check_term(&env.bind_vars(var_binds), &**body);
    },
    spine::Term::Call(ref fun, ref ret_cont, ref args) => {
      errors.extend(check_val(env, fun).into_iter());

      match env.lookup_cont(ret_cont) {
        Some(&arg_count) => if arg_count != 1 {
          errors.push(format!("return args mismatch: '{}' expects {} args",
            ret_cont.0, arg_count))
        },
        None => errors.push(format!("return to undefined cont: '{}'", ret_cont.0)),
      }

      for arg in args.iter() {
        errors.extend(check_val(env, arg).into_iter());
      }
    },
    spine::Term::ExternCall(ref _extern_name, ref ret_cont, ref args) => {
      match env.lookup_cont(ret_cont) {
        Some(&arg_count) => if arg_count != 1 {
          errors.push(format!("extern return args mismatch: '{}' expects {} args",
            ret_cont.0, arg_count))
        },
        None => errors.push(format!("extern return to undefined cont: '{}'", ret_cont.0)),
      }

      for arg in args.iter() {
        errors.extend(check_val(env, arg).into_iter());
      }
    },
    spine::Term::Cont(ref cont, ref args) => {
      match env.lookup_cont(cont) {
        Some(&arg_count) => if arg_count != args.len() {
          errors.push(format!("continue args mismatch: '{}' expects {} args, got {}",
            cont.0, arg_count, args.len()));
        },
        None => errors.push(format!("continue to undefined cont: '{}'", cont.0)),
      }

      for arg in args.iter() {
        errors.extend(check_val(env, arg).into_iter());
      }
    },
    spine::Term::Branch(ref boolval, ref then_cont, ref else_cont) => {
      match env.lookup_cont(then_cont) {
        Some(&arg_count) => if arg_count != 0 {
          errors.push(format!("then-branch args mismatch: '{}' expects {} args",
            then_cont.0, arg_count));
        },
        None => errors.push(format!("then-branch to undefined cont: '{}'", then_cont.0)),
      }

      match env.lookup_cont(else_cont) {
        Some(&arg_count) => if arg_count != 0 {
          errors.push(format!("else-branch args mismatch: '{}' expects {} args",
            else_cont.0, arg_count));
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
    spine::Val::Int(_) | spine::Val::True | spine::Val::False => vec![],
    spine::Val::Var(ref var) => match env.lookup_var(var) {
      Some(_) => vec![],
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
    let errs = spine::check::check(&spine);
    println!("{:?}", errs);
    errs
  }

  #[test]
  fn test_undefined_var() {
    let errs = parse_check("(program halt 
      (letfun (main r () () (cont r x))
        (cont halt 0)))");
    assert_eq!(errs.len(), 1);
    assert!(errs[0].contains("undefined var: 'x'"));
  }

  #[test]
  fn test_undefined_conts() {
    let errs = parse_check(
      "(program halt
        (letfun (f r () (a b) (cont r 100))
          (letcont (bad-call () (call f cc1 1 2))
                   (bad-ext-call () (extern-call ext_add cc2 1 2))
                   (bad-cont () (cont cc3 3))
                   (bad-branch () (branch (is-true 1) cc4 cc5))
            (cont halt 0))))");
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
      "(program halt
        (letfun
          (main r () () (cont r 0))
          (ok-return-42 r () () (cont r 42))
          (bad-return-2 ret () (a b) (cont ret a b))
          (bad-return-0 ret () (a b) (cont ret))
          (bad-call r () () 
            (letcont (needs-two-args (x y) (cont r x))
              (call 100 needs-two-args)))
          (bad-extern-call r () ()
            (letcont (needs-zero-args () (cont r 10))
              (extern-call __boo__ needs-zero-args 1 2 3)))
          (bad-branch r () ()
            (letcont (needs-one-arg (x) (cont r x))
                     (needs-two-args (x y) (cont r y))
              (branch (is-true 0) needs-one-arg needs-two-args)))
          (cont halt)))");
    assert_eq!(errs.len(), 7);
    assert!(errs[0].contains("continue args mismatch: 'ret' expects 1 args"));
    assert!(errs[1].contains("continue args mismatch: 'ret' expects 1 args"));
    assert!(errs[2].contains("return args mismatch: 'needs-two-args' expects 2 args"));
    assert!(errs[3].contains("extern return args mismatch: \
      'needs-zero-args' expects 0 args"));
    assert!(errs[4].contains("then-branch args mismatch: \
      'needs-one-arg' expects 1 args"));
    assert!(errs[5].contains("else-branch args mismatch: \
      'needs-two-args' expects 2 args"));
    assert!(errs[6].contains("continue args mismatch: 'halt' expects 1 args"));
  }

  #[test]
  fn test_undefined_capture() {
    let errs = parse_check("(program halt 
      (letfun (f r (x) (a b) (cont r x))
        (cont halt 0)))");
    assert_eq!(errs.len(), 1);
    assert!(errs[0].contains("undefined capture: 'x'"));
  }

  #[test]
  fn test_non_captured_var() {
    let errs = parse_check("(program halt
      (letobj (double half 0.5)
        (letfun (f r () (a b) (cont r half))
          (cont halt 0))))");
    assert_eq!(errs.len(), 1);
    assert!(errs[0].contains("undefined var: 'half'"));
  }
}
