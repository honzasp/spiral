use std::collections::{HashSet, HashMap};
use spine;
use spiral;

type Res<T> = Result<T, String>;

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

#[derive(Debug, Clone)]
enum FunBind {
  FunName(spine::FunName, usize),
  ExternName(spine::ExternName, usize),
  Prim(PrimFun),
}

#[derive(Copy, Debug, Clone)]
enum PrimFun {
  Add, Sub, Mul, Div,
  Less, LessEq, Eq, NotEq, GreaterEq, Greater,
}

#[derive(Debug)]
enum Env<'p> {
  Empty,
  BindVars(HashMap<spiral::Var, spine::Var>, &'p Env<'p>),
  BindFuns(HashMap<spiral::FunName, FunBind>, &'p Env<'p>),
}

impl<'q> Env<'q> {
  fn bind_vars<'p>(&'p self, binds: Vec<(spiral::Var, spine::Var)>) -> Env<'p> {
    Env::BindVars(binds.into_iter().collect(), self)
  }

  fn bind_funs<'p>(&'p self, binds: Vec<(spiral::FunName, FunBind)>) -> Env<'p> {
    Env::BindFuns(binds.into_iter().collect(), self)
  }

  fn lookup_var(&self, var: &spiral::Var) -> Res<spine::Var> {
    match *self {
      Env::Empty  => Err(format!("Undefined var '{}'", var.0)),
      Env::BindVars(ref map, parent) => match map.get(var) {
        Some(bind) => Ok(bind.clone()),
        None => parent.lookup_var(var),
      },
      Env::BindFuns(_, parent) => parent.lookup_var(var),
    }
  }

  fn lookup_fun(&self, name: &spiral::FunName) -> Res<FunBind> {
    match *self {
      Env::Empty  => Err(format!("Undefined fun name '{}'", name.0)),
      Env::BindFuns(ref map, parent) => match map.get(name) {
        Some(bind) => Ok(bind.clone()),
        None => parent.lookup_fun(name),
      },
      Env::BindVars(_, parent) => parent.lookup_fun(name),
    }
  }

}

pub fn spine_from_spiral(prog: &spiral::Prog) -> Result<spine::ProgDef, String> {
  let empty_env = Env::Empty;
  let global_env = bind_global_env(&empty_env);
  let mut st = ProgSt {
      fun_defs: Vec::new(),
      fun_names: HashSet::new(),
      cont_names: HashSet::new(),
      vars: HashSet::new(),
    };

  let halt_cont = st.gen_cont_name("halt");
  let body_term = try!(translate_stmts(&mut st, &global_env, &prog.body[..], 
    |_, _, _| Ok(spine::Term::Cont(halt_cont.clone(), vec![]))));

  Ok(spine::ProgDef {
      fun_defs: st.fun_defs,
      halt_cont: halt_cont,
      body: body_term,
    })
}

fn translate_expr<F>(st: &mut ProgSt, env: &Env, expr: &spiral::Expr, callback: F)
  -> Res<spine::Term>
  where F: FnOnce(&mut ProgSt, &Env, spine::Expr) -> Res<spine::Term>
{
  match *expr {
    /* TODO: monomorphisation hell
    spiral::Expr::Begin(ref stmts) =>
      return translate_stmts(st, env, &stmts[..], callback),
    spiral::Expr::Let(ref var_binds, ref body_stmts) =>
      return translate_let_expr(st, env, &var_binds[..], &body_stmts[..], callback), 
    spiral::Expr::Call(ref fun_name, ref args) =>
      match try!(env.lookup_fun(fun_name)) {
        FunBind::Prim(prim_fun) => return translate_prim_call_expr(st, env, 
            prim_fun, &args[..], callback),
        _ => { },
      },*/
    spiral::Expr::Var(ref var) =>
      return env.lookup_var(var).and_then(|spine_var| {
          callback(st, env, spine::Expr::Var(spine_var))
        }),
    _ => { },
  }

  let join_cont_name = st.gen_cont_name("join");
  let join_cont_arg = st.gen_var("result");
  let join_cont_def = spine::ContDef {
    name: join_cont_name.clone(),
    args: vec![join_cont_arg.clone()],
    body: try!(callback(st, env, spine::Expr::Var(join_cont_arg))),
  };
  Ok(spine::Term::Letcont(vec![join_cont_def],
    box try!(translate_expr_tail(st, env, expr, join_cont_name))))
}

fn translate_exprs<F>(st: &mut ProgSt, env: &Env, exprs: &[&spiral::Expr], callback: F)
  -> Res<spine::Term>
  where F: FnOnce(&mut ProgSt, &Env, Vec<spine::Expr>) -> Res<spine::Term>
{
  let tmp_vars: Vec<_> = exprs.iter().map(|_| st.gen_var("tmp")).collect();
  let tmp_exprs: Vec<_> = tmp_vars.iter()
      .map(|var| spine::Expr::Var(var.clone())).collect();

  let mut term = try!(callback(st, env, tmp_exprs));
  for (&expr, tmp_var) in exprs.iter().zip(tmp_vars.into_iter()).rev() {
    term = try!(translate_expr(st, env, expr, |_st, _env, spine_expr| {
        Ok(spine::Term::Letval(tmp_var, spine_expr, box term))
      }));
  }

  Ok(term)
}

fn translate_expr_branch<F1, F2>(st: &mut ProgSt, env: &Env, expr: &spiral::Expr,
  true_callback: F1, false_callback: F2)
  -> Res<spine::Term>
  where F1: FnOnce(&mut ProgSt, &Env, spine::Expr) -> Res<spine::Term>,
        F2: FnOnce(&mut ProgSt, &Env, spine::Expr) -> Res<spine::Term>
{
  // TODO: implement some special cases
  translate_expr(st, env, expr, |st, env, spine_expr| {
    let tmp = st.gen_var("branch_value");

    let then_cont = st.gen_cont_name("branch_then");
    let then_cont_def = spine::ContDef {
      name: then_cont.clone(),
      args: vec![],
      body: try!(true_callback(st, env, spine::Expr::Var(tmp.clone()))),
    };

    let else_cont = st.gen_cont_name("branch_else");
    let else_cont_def = spine::ContDef {
      name: else_cont.clone(),
      args: vec![],
      body: try!(false_callback(st, env, spine::Expr::Var(tmp.clone()))),
    };

    let boolexpr = spine::Boolexpr::Compare(spine::Cmp::NotEq,
        box spine::Expr::Var(tmp.clone()), box spine::Expr::Literal(0.0));

    Ok(spine::Term::Letval(tmp, spine_expr,
      box spine::Term::Letcont(vec![then_cont_def],
        box spine::Term::Letcont(vec![else_cont_def],
          box spine::Term::Branch(boolexpr, then_cont, else_cont)))))
  })
}

fn translate_stmts<F>(st: &mut ProgSt, env: &Env, stmts: &[spiral::Stmt], callback: F)
  -> Res<spine::Term>
  where F: FnOnce(&mut ProgSt, &Env, spine::Expr) -> Res<spine::Term>
{
  if stmts.len() == 0 {
    callback(st, env, spine::Expr::Literal(0.0))
  } else if stmts.len() == 1 {
    translate_stmt(st, env, &stmts[0], callback)
  } else {
    translate_stmt(st, env, &stmts[0], |st, env, _result_expr| {
      translate_stmts(st, env, stmts.tail(), callback)
    })
  }
}

fn translate_stmts_tail(st: &mut ProgSt, env: &Env,
  stmts: &[spiral::Stmt], result_cont: spine::ContName) -> Res<spine::Term>
{
  if stmts.len() == 0 {
    Ok(spine::Term::Cont(result_cont, vec![spine::Expr::Literal(0.0)]))
  } else if stmts.len() == 1 {
    translate_stmt(st, env, &stmts[0], |_st, _env, result_expr| {
      Ok(spine::Term::Cont(result_cont, vec![result_expr]))
    })
  } else {
    translate_stmt(st, env, &stmts[0], |st, env, _result_expr| {
      translate_stmts_tail(st, env, stmts.tail(), result_cont)
    })
  }
}

fn translate_stmt<F>(st: &mut ProgSt, env: &Env, stmt: &spiral::Stmt, callback: F)
  -> Res<spine::Term>
  where F: FnOnce(&mut ProgSt, &Env, spine::Expr) -> Res<spine::Term>
{
  match *stmt {
    spiral::Stmt::Fun(ref fun_name, ref args, ref body_stmts) => {
      let spine_name = st.gen_fun_name(&fun_name.0[..]);
      let bind = FunBind::FunName(spine_name.clone(), args.len());
      let inner_env = env.bind_funs(vec![(fun_name.clone(), bind)]);
      try!(translate_fun(st, &inner_env, spine_name, &args[..], &body_stmts[..]));
      callback(st, &inner_env, spine::Expr::Literal(0.0))
    },
    spiral::Stmt::Var(ref var, ref expr) => {
      let spine_var = st.gen_var(&var.0[..]);
      let inner_env = env.bind_vars(vec![(var.clone(), spine_var.clone())]);
      translate_expr(st, &inner_env, expr, |st, env, spine_expr| {
        Ok(spine::Term::Letval(spine_var, spine_expr,
          box try!(callback(st, env, spine::Expr::Literal(0.0)))))
      })
    },
    spiral::Stmt::Expr(ref expr) => 
      translate_expr(st, env, expr, callback),
  }
}

fn translate_fun(st: &mut ProgSt, env: &Env, spine_name: spine::FunName,
  args: &[spiral::Var], body_stmts: &[spiral::Stmt]) -> Res<()>
{
  let args_spine: Vec<_> = args.iter().map(|arg| st.gen_var(&arg.0[..])).collect();
  let arg_binds = args.iter().zip(args_spine.iter())
      .map(|(arg, spine_arg)| (arg.clone(), spine_arg.clone())).collect();
  let inner_env = env.bind_vars(arg_binds);

  let ret_cont = st.gen_cont_name("return");
  let body_spine = try!(translate_stmts_tail(st, &inner_env,
      body_stmts, ret_cont.clone()));

  Ok(st.fun_defs.push(spine::FunDef {
      name: spine_name,
      ret: ret_cont,
      args: args_spine,
      body: body_spine,
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
    spiral::Expr::Begin(ref stmts) =>
      translate_stmts_tail(st, env, &stmts[..], result_cont),
    spiral::Expr::Let(ref var_binds, ref body_stmts) =>
      translate_let_expr(st, env, &var_binds[..], &body_stmts[..],
        |_st, _env, result_expr| Ok(spine::Term::Cont(result_cont, vec![result_expr]))),
    spiral::Expr::Call(ref fun_name, ref args) =>
      translate_call_expr_tail(st, env, fun_name, &args[..], result_cont),
    spiral::Expr::Var(ref var) =>
      env.lookup_var(var).and_then(|spine_var| {
          Ok(spine::Term::Cont(result_cont, vec![spine::Expr::Var(spine_var)]))
        }),
    spiral::Expr::Literal(number) => 
      Ok(spine::Term::Cont(result_cont, vec![spine::Expr::Literal(number)])),
  }
}

fn translate_if_expr_tail(st: &mut ProgSt, env: &Env,
  cond: &spiral::Expr,
  then_e: &spiral::Expr,
  else_e: &spiral::Expr,
  result_cont: spine::ContName) -> Res<spine::Term>
{
  translate_expr_branch(st, env, cond,
    |st, env, _true_expr| translate_expr_tail(st, env, then_e, result_cont.clone()),
    |st, env, _false_expr| translate_expr_tail(st, env, else_e, result_cont.clone()))
}

fn translate_do_expr_tail(st: &mut ProgSt, env: &Env,
  var_binds: &[(spiral::Var, spiral::Expr, spiral::Expr)],
  exit_cond: &spiral::Expr,
  exit_stmts: &[spiral::Stmt],
  body_stmts: &[spiral::Stmt],
  result_cont: spine::ContName) -> Res<spine::Term>
{
  let bound_vars: Vec<_> = var_binds.iter()
    .map(|&(ref var, _, _)| st.gen_var(&var.0[..]))
    .collect();
  let inits: Vec<_> = var_binds.iter().map(|&(_, ref init, _)| init).collect();
  let nexts: Vec<_> = var_binds.iter().map(|&(_, _, ref next)| next).collect();

  let inner_env = env.bind_vars(var_binds.iter().zip(bound_vars.iter())
    .map(|(&(ref spiral_var, _, _), spine_var)| 
         (spiral_var.clone(), spine_var.clone()))
    .collect());

  let next_cont = st.gen_cont_name("do_next");
  let loop_cont = st.gen_cont_name("do_loop");

  let next_cont_arg = st.gen_var("do_ignored");
  let next_cont_def = spine::ContDef {
      name: next_cont.clone(),
      args: vec![next_cont_arg],
      body: try!(translate_exprs(st, &inner_env, &nexts[..], |_st, _env, next_exprs| {
          Ok(spine::Term::Cont(loop_cont.clone(), next_exprs))
        })),
    };

  let loop_cont_def = spine::ContDef {
    name: loop_cont.clone(),
    args: bound_vars.clone(),
    body: try!(translate_expr_branch(st, &inner_env, exit_cond,
      |st, env, _true_expr| 
        translate_stmts_tail(st, env, &exit_stmts[..], result_cont),
      |st, env, _false_expr|
        Ok(spine::Term::Letcont(vec![next_cont_def],
          box try!(translate_stmts_tail(st, env, &body_stmts[..], next_cont)))),
      )),
  };

  translate_exprs(st, env, &inits[..], |_st, _env, init_exprs| {
      Ok(spine::Term::Letcont(vec![loop_cont_def],
        box spine::Term::Cont(loop_cont.clone(), init_exprs)))
    })
}

fn translate_and_expr_tail(st: &mut ProgSt, env: &Env,
  exprs: &[spiral::Expr], result_cont: spine::ContName) -> Res<spine::Term>
{
  if exprs.len() == 0 {
    Ok(spine::Term::Cont(result_cont, vec![spine::Expr::Literal(1.0)]))
  } else if exprs.len() == 1 {
    translate_expr_tail(st, env, &exprs[0], result_cont)
  } else {
    translate_expr(st, env, &exprs[0], |st, env, spine_expr| {
      let tmp = st.gen_var("and_tmp");
      let boolexpr = spine::Boolexpr::Compare(spine::Cmp::Eq, 
          box spine::Expr::Var(tmp.clone()),
          box spine::Expr::Literal(0.0));

      let false_cont = st.gen_cont_name("and_false");
      let false_cont_def = spine::ContDef {
          name: false_cont.clone(),
          args: vec![],
          body: spine::Term::Cont(result_cont.clone(),
              vec![spine::Expr::Var(tmp.clone())]),
        };

      let true_cont = st.gen_cont_name("and_true");
      let true_cont_def = spine::ContDef {
          name: true_cont.clone(),
          args: vec![],
          body: try!(translate_and_expr_tail(st, env, &exprs[1..], result_cont)),
        };
      
      Ok(spine::Term::Letval(tmp, spine_expr,
        box spine::Term::Letcont(vec![false_cont_def],
          box spine::Term::Letcont(vec![true_cont_def],
            box spine::Term::Branch(boolexpr, false_cont, true_cont)))))
    })
  }
}

fn translate_let_expr<F>(st: &mut ProgSt, env: &Env,
  var_binds: &[(spiral::Var, spiral::Expr)],
  body_stmts: &[spiral::Stmt],
  callback: F) -> Res<spine::Term>
  where F: FnOnce(&mut ProgSt, &Env, spine::Expr) -> Res<spine::Term>
{
  match var_binds.first() {
    None => 
      translate_stmts(st, env, body_stmts, callback),
    Some(&(ref var, ref expr)) =>
      translate_expr(st, env, expr, |st, env, spine_expr| {
        let bound_var = st.gen_var(&var.0[..]);
        let bound_env = env.bind_vars(vec![(var.clone(), bound_var.clone())]);
        Ok(spine::Term::Letval(bound_var, spine_expr,
          box try!(translate_let_expr(st, &bound_env,
            var_binds.tail(), body_stmts, callback))))
      })
  }
}

fn translate_call_expr_tail(st: &mut ProgSt, env: &Env,
  fun_name: &spiral::FunName, args: &[spiral::Expr],
  result_cont: spine::ContName) -> Res<spine::Term>
{
  let arg_refs: Vec<_> = args.iter().map(|arg| arg).collect();
  translate_exprs(st, env, &arg_refs[..], |st, env, spine_args| {
    match try!(env.lookup_fun(fun_name)) {
      FunBind::FunName(ref spine_name, argc) =>
        if argc == args.len() {
          Ok(spine::Term::Call(spine_name.clone(), result_cont, spine_args))
        } else {
          Err(format!("fun '{}' expects {} args, got {}",
                      fun_name.0, argc, args.len()))
        },
      FunBind::ExternName(ref extern_name, argc) =>
        if argc == args.len() {
          Ok(spine::Term::ExternCall(extern_name.clone(), result_cont, spine_args))
        } else {
          Err(format!("extern fun '{}' expects {} args, got {}",
                      fun_name.0, argc, args.len()))
        },
      FunBind::Prim(prim_fun) => match prim_fun {
        PrimFun::Add => translate_prim_arithmetic_expr_tail(st, env, spine_args,
            0.0, spine::Binop::Add, result_cont),
        PrimFun::Sub => translate_prim_arithmetic_expr_tail(st, env, spine_args,
            0.0, spine::Binop::Sub, result_cont),
        PrimFun::Mul => translate_prim_arithmetic_expr_tail(st, env, spine_args,
            1.0, spine::Binop::Mul, result_cont),
        PrimFun::Div => translate_prim_arithmetic_expr_tail(st, env, spine_args,
            1.0, spine::Binop::Div, result_cont),
        PrimFun::Less => translate_prim_cmp_expr_tail(st, env, spine_args,
            spine::Cmp::Less, result_cont),
        PrimFun::LessEq => translate_prim_cmp_expr_tail(st, env, spine_args,
            spine::Cmp::LessEq, result_cont),
        PrimFun::Eq => translate_prim_cmp_expr_tail(st, env, spine_args,
            spine::Cmp::Eq, result_cont),
        PrimFun::NotEq => translate_prim_cmp_expr_tail(st, env, spine_args,
            spine::Cmp::NotEq, result_cont),
        PrimFun::GreaterEq => translate_prim_cmp_expr_tail(st, env, spine_args,
            spine::Cmp::GreaterEq, result_cont),
        PrimFun::Greater => translate_prim_cmp_expr_tail(st, env, spine_args,
            spine::Cmp::Greater, result_cont),
      },
    }
  })
}

#[allow(dead_code)]
fn translate_prim_call_expr<F>(st: &mut ProgSt, env: &Env,
  prim_fun: PrimFun, args: &[spiral::Expr], callback: F)
  -> Res<spine::Term>
  where F: FnOnce(&mut ProgSt, &Env, spine::Expr) -> Res<spine::Term>
{
  let arg_refs: Vec<_> = args.iter().map(|arg| arg).collect();
  translate_exprs(st, env, &arg_refs[..], |st, env, spine_args| {
    match prim_fun {
      PrimFun::Add => translate_prim_arithmetic_expr(st, env, spine_args,
          0.0, spine::Binop::Add, callback),
      PrimFun::Sub => translate_prim_arithmetic_expr(st, env, spine_args,
          0.0, spine::Binop::Sub, callback),
      PrimFun::Mul => translate_prim_arithmetic_expr(st, env, spine_args,
          1.0, spine::Binop::Mul, callback),
      PrimFun::Div => translate_prim_arithmetic_expr(st, env, spine_args,
          1.0, spine::Binop::Div, callback),
      PrimFun::Less => translate_prim_cmp_expr(st, env, spine_args,
          spine::Cmp::Less, callback),
      PrimFun::LessEq => translate_prim_cmp_expr(st, env, spine_args,
          spine::Cmp::LessEq, callback),
      PrimFun::Eq => translate_prim_cmp_expr(st, env, spine_args,
          spine::Cmp::Eq, callback),
      PrimFun::NotEq => translate_prim_cmp_expr(st, env, spine_args,
          spine::Cmp::NotEq, callback),
      PrimFun::GreaterEq => translate_prim_cmp_expr(st, env, spine_args,
          spine::Cmp::GreaterEq, callback),
      PrimFun::Greater => translate_prim_cmp_expr(st, env, spine_args,
          spine::Cmp::Greater, callback),
    }
  })
}

fn translate_prim_arithmetic_expr_tail(st: &mut ProgSt, env: &Env,
  args: Vec<spine::Expr>, identity: f32, binop: spine::Binop,
  result_cont: spine::ContName) -> Res<spine::Term>
{
  translate_prim_arithmetic_expr(st, env, args, identity, binop,
    |_st, _env, expr| { Ok(spine::Term::Cont(result_cont, vec![expr])) })
}

fn translate_prim_arithmetic_expr<F>(st: &mut ProgSt, env: &Env,
  mut args: Vec<spine::Expr>, _identity: f32, binop: spine::Binop,
  callback: F) -> Res<spine::Term>
  where F: FnOnce(&mut ProgSt, &Env, spine::Expr) -> Res<spine::Term>
{
  if args.len() == 2 {
    let right = args.pop().unwrap();
    let left = args.pop().unwrap();
    callback(st, env, spine::Expr::Binary(binop, box left, box right))
  } else {
    Err(format!("non-binary arithmetic is not implemented"))
  }
}

fn translate_prim_cmp_expr_tail(st: &mut ProgSt, env: &Env,
  args: Vec<spine::Expr>, cmp: spine::Cmp,
  result_cont: spine::ContName) -> Res<spine::Term>
{
  translate_prim_cmp_branch(st, env, args, cmp,
    |_st, _env, _true_expr|
      Ok(spine::Term::Cont(result_cont.clone(), vec![spine::Expr::Literal(1.0)])),
    |_st, _env, _false_expr|
      Ok(spine::Term::Cont(result_cont.clone(), vec![spine::Expr::Literal(0.0)])),
    )
}

#[allow(dead_code)]
fn translate_prim_cmp_expr<F>(st: &mut ProgSt, env: &Env,
  args: Vec<spine::Expr>, cmp: spine::Cmp,
  callback: F) -> Res<spine::Term>
  where F: FnOnce(&mut ProgSt, &Env, spine::Expr) -> Res<spine::Term>
{
  let join_cont = st.gen_cont_name("cmp_join");
  let join_cont_arg = st.gen_var("cmp_join_arg");
  let join_cont_def = spine::ContDef {
      name: join_cont.clone(),
      args: vec![join_cont_arg.clone()],
      body: try!(callback(st, env, spine::Expr::Var(join_cont_arg))),
    };

  Ok(spine::Term::Letcont(vec![join_cont_def],
    box try!(translate_prim_cmp_branch(st, env, args, cmp,
      |_st, _env, true_expr|
        Ok(spine::Term::Cont(join_cont.clone(), vec![true_expr])),
      |_st, _env, false_expr|
        Ok(spine::Term::Cont(join_cont.clone(), vec![false_expr])),
    ))))
}

fn translate_prim_cmp_branch<F1, F2>(st: &mut ProgSt, env: &Env,
  mut args: Vec<spine::Expr>, cmp: spine::Cmp,
  true_callback: F1, false_callback: F2)
  -> Res<spine::Term>
  where F1: FnOnce(&mut ProgSt, &Env, spine::Expr) -> Res<spine::Term>,
        F2: FnOnce(&mut ProgSt, &Env, spine::Expr) -> Res<spine::Term>
{
  if args.len() == 2 {
    let true_cont = st.gen_cont_name("cmp_true");
    let true_cont_def = spine::ContDef {
        name: true_cont.clone(),
        args: vec![],
        body: try!(true_callback(st, env, spine::Expr::Literal(1.0))),
      };

    let false_cont = st.gen_cont_name("cmp_false");
    let false_cont_def = spine::ContDef {
        name: false_cont.clone(),
        args: vec![],
        body: try!(false_callback(st, env, spine::Expr::Literal(0.0))),
      };

    let right = args.pop().unwrap();
    let left = args.pop().unwrap();
    let boolexpr = spine::Boolexpr::Compare(cmp, box left, box right);
    Ok(spine::Term::Letcont(vec![false_cont_def],
      box spine::Term::Letcont(vec![true_cont_def],
        box spine::Term::Branch(boolexpr, true_cont, false_cont))))
  } else {
    Err(format!("non-binary comparison is not implemented"))
  }
}

fn bind_global_env<'p>(parent: &'p Env) -> Env<'p> {
  let prims = &[
      ("+", PrimFun::Add),
      ("-", PrimFun::Sub),
      ("*", PrimFun::Mul),
      ("/", PrimFun::Div),
      ("<", PrimFun::Less),
      ("<=", PrimFun::LessEq),
      ("=", PrimFun::Eq),
      ("/=", PrimFun::NotEq),
      (">=", PrimFun::GreaterEq),
      (">", PrimFun::Greater),
    ];

  let externs = &[
      ("println", "spiral_ext_println", 1),
    ];

  let mut global_funs = Vec::new();
  for &(name, prim) in prims.iter() {
    global_funs.push((spiral::FunName(name.to_string()), FunBind::Prim(prim)));
  }
  for &(name, extern_name, argc) in externs.iter() {
    let extern_name = spine::ExternName(extern_name.to_string());
    global_funs.push((spiral::FunName(name.to_string()),
      FunBind::ExternName(extern_name, argc)));
  }
  parent.bind_funs(global_funs)
}
