use std::collections::{HashSet};
use spine;
use spiral;
use spine::onion::{Onion, OnionContDef};

type Res<T> = Result<T, String>;
type Env = spiral::env::Env<spine::Val, FunBind>;

enum StmtRes {
  Env(Env),
  Val(spine::Val),
  Empty,
}

impl StmtRes {
  fn into_val(self) -> spine::Val {
    match self {
      StmtRes::Val(val) => val,
      StmtRes::Env(_) | StmtRes::Empty => spine::Val::Literal(0.0),
    }
  }
}

#[derive(Debug, Clone)]
enum FunBind {
  FunName(spine::FunName, usize),
  ExternName(spine::ExternName, usize),
}

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

pub fn spine_from_spiral(prog: &spiral::Prog) -> Result<spine::ProgDef, String> {
  let empty_env = spiral::env::Env::new();
  let global_env = bind_global_env(empty_env);
  let mut st = ProgSt {
      fun_defs: Vec::new(),
      fun_names: HashSet::new(),
      cont_names: HashSet::new(),
      vars: HashSet::new(),
    };

  let halt_cont = st.gen_cont_name("halt");
  let main_name = st.gen_fun_name("main");
  let (main_onion, main_res) = try!(translate_stmts(&mut st, &global_env,
      &prog.body[..]));
  let main_term = main_onion.subst_term(
    spine::Term::Cont(halt_cont.clone(), vec![main_res.into_val()]));

  st.fun_defs.push(spine::FunDef {
      name: main_name.clone(),
      ret: halt_cont,
      args: vec![],
      body: main_term,
    });

  Ok(spine::ProgDef {
      fun_defs: st.fun_defs,
      main_fun: main_name.clone(),
    })
}

fn translate_expr(st: &mut ProgSt, env: &Env, expr: &spiral::Expr)
  -> Res<(Onion, spine::Val)>
{
  match *expr {
    spiral::Expr::Begin(ref stmts) => {
      let (onion, stmt_res) = try!(translate_stmts(st, env, &stmts[..]));
      Ok((onion, stmt_res.into_val()))
    },
    spiral::Expr::Let(ref var_binds, ref body_stmts) =>
      translate_let_expr(st, env, &var_binds[..], &body_stmts[..]), 
    spiral::Expr::Literal(number) =>
      Ok((Onion::Hole, spine::Val::Literal(number))),
    spiral::Expr::Var(ref var) => match env.lookup_var(var) {
      Some(spine_val) =>
        return Ok((Onion::Hole, spine_val.clone())),
      None =>
        return Err(format!("undefined var '{}'", var.0)),
    },
    _ => {
      let join_cont_name = st.gen_cont_name("join");
      let join_cont_arg = st.gen_var("result");

      Ok((Onion::Letjoin(box OnionContDef {
            name: join_cont_name.clone(),
            args: vec![join_cont_arg.clone()],
            body: Onion::Hole,
          }, box try!(translate_expr_tail(st, env, expr, join_cont_name))), 
        spine::Val::Var(join_cont_arg)))
    },
  }
}

fn translate_exprs(st: &mut ProgSt, env: &Env, exprs: &[&spiral::Expr])
  -> Res<(Onion, Vec<spine::Val>)>
{
  exprs.iter().fold(Ok((Onion::Hole, Vec::new())), |fold_val, expr| {
    let (onion, mut vals) = try!(fold_val);
    let (expr_onion, expr_val) = try!(translate_expr(st, env, expr));
    vals.push(expr_val);
    Ok((onion.subst_onion(expr_onion), vals))
  })
}

fn translate_stmts(st: &mut ProgSt, env: &Env, stmts: &[spiral::Stmt])
  -> Res<(Onion, StmtRes)>
{
  if stmts.len() == 0 {
    Ok((Onion::Hole, StmtRes::Empty))
  } else if stmts.len() == 1 {
    translate_stmt(st, env, &stmts[0])
  } else {
    let (stmt_onion, stmt_res) = try!(translate_stmt(st, env, &stmts[0]));
    let (next_onion, next_stmt_res) = try!(match stmt_res {
      StmtRes::Env(ref inner_env) => translate_stmts(st, inner_env, &stmts[1..]),
      StmtRes::Val(_) | StmtRes::Empty => translate_stmts(st, env, &stmts[1..]),
    });
    Ok((stmt_onion.subst_onion(next_onion), next_stmt_res))
  }
}

fn translate_stmt(st: &mut ProgSt, env: &Env, stmt: &spiral::Stmt)
  -> Res<(Onion, StmtRes)>
{
  match *stmt {
    spiral::Stmt::Fun(ref fun_name, ref args, ref body_stmts) => {
      let spine_name = st.gen_fun_name(&fun_name.0[..]);
      let bind = FunBind::FunName(spine_name.clone(), args.len());
      let inner_env = env.bind_funs(vec![(fun_name.clone(), bind)]);
      try!(translate_fun(st, &inner_env, spine_name, &args[..], &body_stmts[..]));
      Ok((Onion::Hole, StmtRes::Env(inner_env)))
    },
    spiral::Stmt::Var(ref var, ref expr) => {
      let (expr_onion, expr_val) = try!(translate_expr(st, env, expr));
      let inner_env = env.bind_vars(vec![(var.clone(), expr_val)]);
      Ok((expr_onion, StmtRes::Env(inner_env)))
    },
    spiral::Stmt::Expr(ref expr) => {
      let (expr_onion, expr_val) = try!(translate_expr(st, env, expr));
      Ok((expr_onion, StmtRes::Val(expr_val)))
    },
  }
}

fn translate_fun(st: &mut ProgSt, env: &Env, spine_name: spine::FunName,
  args: &[spiral::Var], body_stmts: &[spiral::Stmt]) -> Res<()>
{
  let spine_args: Vec<_> = args.iter().map(|arg| st.gen_var(&arg.0[..])).collect();
  let arg_binds = args.iter().zip(spine_args.iter())
      .map(|(arg, spine_arg)| (arg.clone(), spine::Val::Var(spine_arg.clone())))
      .collect();
  let inner_env = env.bind_vars(arg_binds);

  let ret_cont = st.gen_cont_name("return");
  let (body_onion, body_stmt_res) = try!(translate_stmts(st, &inner_env, body_stmts));

  Ok(st.fun_defs.push(spine::FunDef {
      name: spine_name,
      ret: ret_cont.clone(),
      args: spine_args,
      body: body_onion.subst_term(spine::Term::Cont(ret_cont,
        vec![body_stmt_res.into_val()])),
    }))
}

fn translate_expr_tail(st: &mut ProgSt, env: &Env,
  expr: &spiral::Expr, result_cont: spine::ContName) -> Res<spine::Term>
{
  match *expr {
    spiral::Expr::If(ref cond, ref then_e, ref else_e) =>
      translate_if_expr_tail(st, env, &**cond, &**then_e, &**else_e, result_cont),
    spiral::Expr::Cond(_, _) =>
      Err(format!("'cond' not implemented")),
    spiral::Expr::When(_, _) =>
      Err(format!("'when' not implemented")),
    spiral::Expr::Unless(_, _) =>
      Err(format!("'unless' not implemented")),
    spiral::Expr::Do(ref var_binds, ref exit_cond, ref exit_stmts, ref body_stmts) => 
      translate_do_expr_tail(st, env,
        &var_binds[..], &**exit_cond, &exit_stmts[..], &body_stmts[..],
        result_cont),
    spiral::Expr::And(ref exprs) => 
      translate_and_expr_tail(st, env, &exprs[..], result_cont),
    spiral::Expr::Or(_) =>
      Err(format!("'or' not implemented")),
    spiral::Expr::Begin(ref stmts) => {
      let (onion, stmt_res) = try!(translate_stmts(st, env, &stmts[..]));
      Ok(onion.subst_term(spine::Term::Cont(result_cont, vec![stmt_res.into_val()])))
    },
    spiral::Expr::Let(ref var_binds, ref body_stmts) => {
      let (onion, val) = try!(translate_let_expr(st, env, &var_binds[..], &body_stmts[..]));
      Ok(onion.subst_term(spine::Term::Cont(result_cont, vec![val])))
    },
    spiral::Expr::Call(ref fun_name, ref args) =>
      translate_call_expr_tail(st, env, fun_name, &args[..], result_cont),
    spiral::Expr::Var(ref var) => match env.lookup_var(var) {
      Some(spine_val) =>
        Ok(spine::Term::Cont(result_cont, vec![spine_val.clone()])),
      None =>
        Err(format!("undefined var '{}'", var.0)),
    },
    spiral::Expr::Literal(number) => 
      Ok(spine::Term::Cont(result_cont, vec![spine::Val::Literal(number)])),
  }
}

fn translate_if_expr_tail(st: &mut ProgSt, env: &Env,
  cond: &spiral::Expr,
  then_e: &spiral::Expr,
  else_e: &spiral::Expr,
  result_cont: spine::ContName) -> Res<spine::Term>
{
  let (cond_onion, cond_val) = try!(translate_expr(st, env, cond));
  let then_term = try!(translate_expr_tail(st, env, then_e, result_cont.clone()));
  let else_term = try!(translate_expr_tail(st, env, else_e, result_cont));

  let then_cont = st.gen_cont_name("then");
  let then_cont_def = spine::ContDef {
      name: then_cont.clone(),
      args: vec![],
      body: then_term,
    };

  let else_cont = st.gen_cont_name("else");
  let else_cont_def = spine::ContDef {
      name: else_cont.clone(),
      args: vec![],
      body: else_term,
    };

  Ok(cond_onion.subst_term(
    spine::Term::Letcont(vec![then_cont_def], 
      box spine::Term::Letcont(vec![else_cont_def],
        box spine::Term::Branch(spine::Boolval::IsTrue(cond_val),
          then_cont, else_cont)))))
}

fn translate_do_expr_tail(st: &mut ProgSt, env: &Env,
  var_binds: &[(spiral::Var, spiral::Expr, spiral::Expr)],
  exit_cond: &spiral::Expr,
  exit_stmts: &[spiral::Stmt],
  body_stmts: &[spiral::Stmt],
  result_cont: spine::ContName) -> Res<spine::Term>
{
  let spine_vars: Vec<_> = var_binds.iter()
    .map(|&(ref var, _, _)| st.gen_var(&var.0[..]))
    .collect();
  let inits: Vec<_> = var_binds.iter().map(|&(_, ref init, _)| init).collect();
  let nexts: Vec<_> = var_binds.iter().map(|&(_, _, ref next)| next).collect();

  let inner_env = env.bind_vars(var_binds.iter().zip(spine_vars.iter())
    .map(|(&(ref spiral_var, _, _), spine_var)| 
         (spiral_var.clone(), spine::Val::Var(spine_var.clone())))
    .collect());

  let next_cont = st.gen_cont_name("do_next");
  let loop_cont = st.gen_cont_name("do_loop");
  let exit_cont = st.gen_cont_name("do_exit");

  let next_cont_def = spine::ContDef {
      name: next_cont.clone(),
      args: vec![],
      body: {
        let (nexts_onion, next_vals) = try!(translate_exprs(st, &inner_env, &nexts[..]));
        let (body_onion, _) = try!(translate_stmts(st, &inner_env, body_stmts));
        body_onion.subst_term(
          nexts_onion.subst_term(spine::Term::Cont(loop_cont.clone(), next_vals)))
      }
    };

  let exit_cont_def = spine::ContDef {
      name: exit_cont.clone(),
      args: vec![],
      body: {
        let (exit_onion, exit_stmt_res) = try!(translate_stmts(st, &inner_env, exit_stmts));
        exit_onion.subst_term(spine::Term::Cont(result_cont,
          vec![exit_stmt_res.into_val()]))
      }
    };

  let loop_cont_def = spine::ContDef {
      name: loop_cont.clone(),
      args: spine_vars.clone(),
      body: {
        let (cond_onion, cond_val) = try!(translate_expr(st, &inner_env, exit_cond));
        cond_onion.subst_term(
          spine::Term::Letcont(vec![next_cont_def],
            box spine::Term::Letcont(vec![exit_cont_def],
              box spine::Term::Branch(spine::Boolval::IsFalse(cond_val),
                next_cont, exit_cont))))
      }
    };

  let (inits_onion, init_vals) = try!(translate_exprs(st, env, &inits[..]));
  Ok(inits_onion.subst_term(
    spine::Term::Letcont(vec![loop_cont_def],
      box spine::Term::Cont(loop_cont.clone(), init_vals))))
}

fn translate_and_expr_tail(st: &mut ProgSt, env: &Env,
  exprs: &[spiral::Expr], result_cont: spine::ContName) -> Res<spine::Term>
{
  if exprs.len() == 0 {
    Ok(spine::Term::Cont(result_cont, vec![spine::Val::Literal(1.0)]))
  } else if exprs.len() == 1 {
    translate_expr_tail(st, env, &exprs[0], result_cont)
  } else {
    let (onion, val) = try!(translate_expr(st, env, &exprs[0]));

    let stop_cont = st.gen_cont_name("and_stop");
    let stop_cont_def = spine::ContDef {
        name: stop_cont.clone(),
        args: vec![],
        body: spine::Term::Cont(result_cont.clone(), vec![val.clone()]),
      };

    let continue_cont = st.gen_cont_name("and_continue");
    let continue_cont_def = spine::ContDef {
        name: continue_cont.clone(),
        args: vec![],
        body: try!(translate_and_expr_tail(st, env, &exprs[1..], result_cont)),
      };
    
    Ok(spine::Term::Letcont(vec![stop_cont_def],
        box spine::Term::Letcont(vec![continue_cont_def],
          box onion.subst_term(spine::Term::Branch(
            spine::Boolval::IsTrue(val.clone()), continue_cont, stop_cont)))))
  }
}

fn translate_let_expr(st: &mut ProgSt, env: &Env,
  var_binds: &[(spiral::Var, spiral::Expr)],
  body_stmts: &[spiral::Stmt]) -> Res<(Onion, spine::Val)>
{
  match var_binds.first() {
    None => {
      let (stmts_onion, stmt_res) = try!(translate_stmts(st, env, body_stmts));
      Ok((stmts_onion, stmt_res.into_val()))
    },
    Some(&(ref var, ref expr)) => {
      let (expr_onion, expr_val) = try!(translate_expr(st, env, expr));
      let inner_env = env.bind_vars(vec![(var.clone(), expr_val)]);
      let (inner_onion, result) = try!(translate_let_expr(st, &inner_env,
          var_binds.tail(), body_stmts));
      Ok((expr_onion.subst_onion(inner_onion), result))
    },
  }
}

fn translate_call_expr_tail(st: &mut ProgSt, env: &Env,
  fun_name: &spiral::FunName, args: &[spiral::Expr],
  result_cont: spine::ContName) -> Res<spine::Term>
{
  let arg_refs: Vec<_> = args.iter().map(|arg| arg).collect();
  let (args_onion, arg_vals) = try!(translate_exprs(st, env, &arg_refs[..]));

  match env.lookup_fun(fun_name) {
    None => Err(format!("undefined fun '{}'", fun_name.0)),
    Some(fun_bind) => match fun_bind {
      &FunBind::FunName(ref spine_name, argc) =>
        if argc == args.len() {
          Ok(args_onion.subst_term(
              spine::Term::Call(spine_name.clone(), result_cont, arg_vals)))
        } else {
          Err(format!("fun '{}' expects {} args, got {}",
                      fun_name.0, argc, args.len()))
        },
      &FunBind::ExternName(ref extern_name, argc) =>
        if argc == args.len() {
          Ok(args_onion.subst_term(
              spine::Term::ExternCall(extern_name.clone(), result_cont, arg_vals)))
        } else {
          Err(format!("extern fun '{}' expects {} args, got {}",
                      fun_name.0, argc, args.len()))
        },
    },
  }
}

fn bind_global_env(parent: Env) -> Env {
  let externs = &[
      ("println", "spiral_ext_println", 1),
      ("__out", "__test_out", 1),
      ("+", "spiral_ext_add", 2),
      ("-", "spiral_ext_sub", 2),
      ("*", "spiral_ext_mul", 2),
      ("/", "spiral_ext_div", 2),
      ("<", "spiral_ext_lt", 2),
      ("<=", "spiral_ext_le", 2),
      ("==", "spiral_ext_eq", 2),
      ("/=", "spiral_ext_ne", 2),
      (">", "spiral_ext_gt", 2),
      (">=", "spiral_ext_ge", 2),
    ];

  parent.bind_funs(externs.iter()
      .map(|&(name, extern_name, argc)| {
        let fun = spiral::FunName(name.to_string());
        let ext = spine::ExternName(extern_name.to_string());
        (fun, FunBind::ExternName(ext, argc))
      }).collect())
}

#[cfg(test)]
mod test {
  use sexpr;
  use spine;
  use spiral;

  fn run(txt: &str) -> Vec<f32> {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    let spiral = sexpr::to_spiral::prog_from_sexpr(&sexpr).unwrap();
    let spine = spiral::to_spine::spine_from_spiral(&spiral).unwrap();
    let errors = spine::check::check_prog(&spine);
    if !errors.is_empty() {
      panic!("spine invalid: {:?}", errors)
    }
    println!("{:?}", spine);
    spine::eval::eval(&spine)
  }

  #[test]
  fn test_empty_program() {
    assert_eq!(run("(program)"), vec![]);
  }

  #[test]
  fn test_output() {
    assert_eq!(run("(program (__out 1) (__out 2))"), vec![1.0, 2.0]);
  }

  #[test]
  fn test_if() {
    assert_eq!(run("(program (__out (if 0 10 20)) (__out (if 4 10 20)))"),
      vec![20.0, 10.0]);
  }

  #[test] #[ignore]
  fn test_cond() {
    assert_eq!(run("(program (__out (cond ((= 1 2) 99) ((< 1 2) 42) (1 66))))"),
      vec![42.0]);
    assert_eq!(run("(program (__out (cond ((= 1 2) 99) ((> 1 2) 33) (else 42))))"),
      vec![42.0]);
  }

  #[test] #[ignore]
  fn test_when() {
    assert_eq!(run("(program (when (= 2 2) (__out 4)) (when 0 (__out 3)))"),
      vec![4.0]);
    assert_eq!(run("(program (when (= 4 4) (__out 1) (__out 2)) (__out 4))"),
      vec![1.0, 2.0, 4.0]);
    assert_eq!(run("(program (when (< 4 4) (__out 1) (__out 2)) (__out 4))"),
      vec![4.0]);
  }

  #[test] #[ignore]
  fn test_unless() {
    assert_eq!(run("(program (unless (= 2 2) (__out 4)) (unless 0 (__out 3)))"),
      vec![4.0]);
    assert_eq!(run("(program (unless (= 4 4) (__out 1) (__out 2)) (__out 4))"),
      vec![4.0]);
    assert_eq!(run("(program (unless (< 4 4) (__out 1) (__out 2)) (__out 4))"),
      vec![1.0, 2.0, 4.0]);
  }

  #[test]
  fn test_do() {
    assert_eq!(run(
      "(program (do ((f1 0 f2) (f2 1 (+ f1 f2)) (i 0 (+ i 1)))
                    ((>= i 10))
                  (__out f1)))"),
      vec![0.0, 1.0, 1.0, 2.0, 3.0, 5.0, 8.0, 13.0, 21.0, 34.0]);
  }

  #[test]
  fn test_begin() {
    assert_eq!(run("(program 
        (__out 2)
        (begin
          (var x 1)
          (var y 3)
          (__out (+ x y)))
        (__out 8))"),
      vec![2.0, 4.0, 8.0]);
  }

  #[test]
  fn test_let() {
    assert_eq!(run("(program
        (__out
          (let ((x 1) (y (+ x 1)) (z (+ y 1)) (w (+ z 1)))
              w)))"),
      vec![4.0]);
  }

  #[test]
  fn test_call() {
    assert_eq!(run("(program
        (fun square (x) (* x x))
        (fun neg (x) (- 0 x))
        (__out (neg (square 2))))"),
      vec![-4.0]);
  }

  #[test]
  fn test_recursive_fun() {
    assert_eq!(run("(program
        (fun fac (x)
          (if (<= x 1) 1
            (* x (fac (- x 1)))))
        (__out (fac 6))) "),
      vec![720.0]);
  }

  #[test] #[ignore]
  fn test_mutually_recursive_funs() {
    assert_eq!(run("(program
        (fun ping (a b)
          (__out a)
          (pong b a))
        (fun pong (a b)
          (if (= a b) 10
            (ping (+ a 1) (- b 2))))
        (__out (pong 6 7)))"),
      vec![7.0, 6.0, 6.0, 5.0, 5.0, 4.0, 10.0]);
  }
}
