use std::collections::{HashSet};
use spine;
use spiral;
use spine::onion::{Onion, OnionContDef};

type Res<T> = Result<T, String>;
type Env = spiral::env::Env<spine::Val>;

#[derive(Debug, Clone)]
enum StmtRes {
  Env(Env),
  Val(spine::Val),
  Empty,
}

impl StmtRes {
  fn into_val(self) -> spine::Val {
    match self {
      StmtRes::Val(val) => val,
      StmtRes::Env(_) | StmtRes::Empty => spine::Val::False,
    }
  }
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
  let mut st = ProgSt {
      fun_defs: Vec::new(),
      fun_names: HashSet::new(),
      cont_names: HashSet::new(),
      vars: HashSet::new(),
    };

  let empty_env = spiral::env::Env::new();
  let global_env = bind_global_env(&mut st, empty_env);

  let halt_cont = st.gen_cont_name("halt");
  let main_name = st.gen_fun_name("main");
  let (main_onion, main_res) = try!(translate_stmts(&mut st, &global_env,
      &prog.body[..]));
  let main_term = main_onion.subst_term(
    spine::Term::Cont(halt_cont.clone(), vec![main_res.into_val()]));

  st.fun_defs.push(spine::FunDef {
      name: main_name.clone(),
      ret: halt_cont,
      captures: vec![],
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
    spiral::Expr::Lambda(ref args, ref body_stmts) =>
      translate_lambda_expr(st, env, &args[..], &body_stmts[..]),
    spiral::Expr::Int(number) =>
      Ok((Onion::Hole, spine::Val::Int(number))),
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
    let fun_defs: Vec<_> = stmts.iter()
      .take_while(|&stmt| match *stmt {
        spiral::Stmt::Fun(_) => true,
        spiral::Stmt::Var(_, _) | spiral::Stmt::Expr(_) => false,
      }).map(|stmt| match *stmt {
        spiral::Stmt::Fun(ref fun_def) => fun_def,
        _ => panic!("non-fun stmts should have been filtered"),
      }).collect();

    let ((stmt_onion, stmt_res), next_stmts) =
      if !fun_defs.is_empty() {
        (try!(translate_fun_stmts(st, env, &fun_defs[..])),
          &stmts[fun_defs.len()..])
      } else {
        (try!(translate_stmt(st, env, &stmts[0])), &stmts[1..])
      };

    let (next_onion, next_stmt_res) = try!(match stmt_res {
      StmtRes::Env(ref inner_env) => translate_stmts(st, inner_env, next_stmts),
      StmtRes::Val(_) | StmtRes::Empty => translate_stmts(st, env, next_stmts),
    });
    Ok((stmt_onion.subst_onion(next_onion), next_stmt_res))
  }
}

fn translate_fun_stmts(st: &mut ProgSt, env: &Env, fun_defs: &[&spiral::FunDef])
  -> Res<(Onion, StmtRes)>
{
  let spine_vars: Vec<_> = fun_defs.iter().map(|fun_def| {
      st.gen_var(&fun_def.var.0[..])
    }).collect();
  let spine_names: Vec<_> = fun_defs.iter().map(|fun_def| {
      st.gen_fun_name(&fun_def.var.0[..])
    }).collect();

  let inner_env = env.bind_vars(fun_defs.iter().zip(spine_vars.iter())
      .map(|(fun_def, spine_var)| {
        (fun_def.var.clone(), spine::Val::Var(spine_var.clone()))
      }).collect());

  let mut clos_defs = Vec::new();
  for (fun_def, (spine_var, spine_name)) in fun_defs.iter()
    .zip(spine_vars.into_iter().zip(spine_names.into_iter()))
  {
    clos_defs.push(try!(translate_fun(st, &inner_env, spine_var, spine_name,
        &fun_def.args[..], &fun_def.body[..])));
  }

  Ok((Onion::Letclos(clos_defs, box Onion::Hole), StmtRes::Env(inner_env)))
}

fn translate_stmt(st: &mut ProgSt, env: &Env, stmt: &spiral::Stmt)
  -> Res<(Onion, StmtRes)>
{
  match *stmt {
    spiral::Stmt::Fun(ref fun_def) =>
      translate_fun_stmts(st, env, &[fun_def]),
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

fn translate_fun(st: &mut ProgSt, env: &Env,
  spine_var: spine::Var, spine_name: spine::FunName,
  args: &[spiral::Var], body_stmts: &[spiral::Stmt]) -> Res<spine::ClosureDef>
{
  let spine_args: Vec<_> = args.iter().map(|arg| st.gen_var(&arg.0[..])).collect();
  let arg_binds = args.iter().zip(spine_args.iter())
      .map(|(arg, spine_arg)| (arg.clone(), spine::Val::Var(spine_arg.clone())))
      .collect();
  let inner_env = env.bind_vars(arg_binds);

  let ret_cont = st.gen_cont_name("return");
  let (body_onion, body_stmt_res) =
    try!(translate_stmts(st, &inner_env, body_stmts));
  let body_term = body_onion.subst_term(spine::Term::Cont(ret_cont.clone(),
    vec![body_stmt_res.into_val()]));

  let mut body_free = spine::free::collect_term(&body_term);
  for arg in spine_args.iter() {
    body_free.remove(arg);
  }
  let captured_vars: Vec<_> = body_free.into_iter().collect();

  st.fun_defs.push(spine::FunDef {
      name: spine_name.clone(),
      ret: ret_cont,
      captures: captured_vars.clone(),
      args: spine_args,
      body: body_term,
    });

  Ok(spine::ClosureDef {
    var: spine_var,
    fun_name: spine_name,
    captures: captured_vars.into_iter().map(spine::Val::Var).collect(),
  })
}

fn translate_expr_tail(st: &mut ProgSt, env: &Env,
  expr: &spiral::Expr, result_cont: spine::ContName) -> Res<spine::Term>
{
  match *expr {
    spiral::Expr::If(ref cond, ref then_e, ref else_e) =>
      translate_if_expr_tail(st, env, &**cond, &**then_e, &**else_e, result_cont),
    spiral::Expr::Cond(ref arms) =>
      translate_cond_expr_tail(st, env, &arms[..], result_cont),
    spiral::Expr::When(ref cond, ref body_stmts) =>
      translate_when_expr_tail(st, env, &cond, &body_stmts[..], result_cont),
    spiral::Expr::Unless(ref cond, ref body_stmts) =>
      translate_unless_expr_tail(st, env, &cond, &body_stmts[..], result_cont),
    spiral::Expr::Do(ref var_binds, ref exit_cond, ref exit_stmts, ref body_stmts) => 
      translate_do_expr_tail(st, env,
        &var_binds[..], &**exit_cond, &exit_stmts[..], &body_stmts[..],
        result_cont),
    spiral::Expr::And(ref exprs) => 
      translate_and_expr_tail(st, env, &exprs[..], result_cont),
    spiral::Expr::Or(ref exprs) =>
      translate_or_expr_tail(st, env, &exprs[..], result_cont),
    spiral::Expr::Begin(ref stmts) => {
      let (onion, stmt_res) = try!(translate_stmts(st, env, &stmts[..]));
      Ok(onion.subst_term(spine::Term::Cont(result_cont, vec![stmt_res.into_val()])))
    },
    spiral::Expr::Let(ref var_binds, ref body_stmts) => {
      let (onion, val) = try!(translate_let_expr(st, env, &var_binds[..], &body_stmts[..]));
      Ok(onion.subst_term(spine::Term::Cont(result_cont, vec![val])))
    },
    spiral::Expr::Call(ref fun, ref args) =>
      translate_call_expr_tail(st, env, fun, &args[..], result_cont),
    spiral::Expr::Lambda(ref args, ref body_stmts) => {
      let (onion, val) = try!(translate_lambda_expr(st, env, &args[..], &body_stmts[..]));
      Ok(onion.subst_term(spine::Term::Cont(result_cont, vec![val])))
    },
    spiral::Expr::Var(ref var) => match env.lookup_var(var) {
      Some(spine_val) =>
        Ok(spine::Term::Cont(result_cont, vec![spine_val.clone()])),
      None =>
        Err(format!("undefined var '{}'", var.0)),
    },
    spiral::Expr::Int(number) => 
      Ok(spine::Term::Cont(result_cont, vec![spine::Val::Int(number)])),
  }
}

fn translate_branch_expr_tail<F1, F2>(
  st: &mut ProgSt, env: &Env,
  cond: &spiral::Expr,
  then_branch: F1,
  else_branch: F2) -> Res<spine::Term>
where F1: Fn(&mut ProgSt, &Env, spine::Val) -> Res<spine::Term>,
      F2: Fn(&mut ProgSt, &Env, spine::Val) -> Res<spine::Term>,
{
  let (cond_onion, cond_val) = try!(translate_expr(st, env, cond));

  let then_cont = st.gen_cont_name("branch-then");
  let then_cont_def = spine::ContDef {
      name: then_cont.clone(),
      args: vec![],
      body: try!(then_branch(st, env, cond_val.clone())),
    };

  let else_cont = st.gen_cont_name("branch-else");
  let else_cont_def = spine::ContDef {
      name: else_cont.clone(),
      args: vec![],
      body: try!(else_branch(st, env, cond_val.clone())),
    };

  Ok(cond_onion.subst_term(
    spine::Term::Letcont(vec![then_cont_def],
      box spine::Term::Letcont(vec![else_cont_def],
        box spine::Term::Branch(spine::Boolval::IsTrue(cond_val),
          then_cont, else_cont)))))
}

fn translate_if_expr_tail(st: &mut ProgSt, env: &Env,
  cond: &spiral::Expr,
  then_e: &spiral::Expr,
  else_e: &spiral::Expr,
  result_cont: spine::ContName) -> Res<spine::Term>
{
  translate_branch_expr_tail(st, env, cond,
    |st, env, _| translate_expr_tail(st, env, then_e, result_cont.clone()),
    |st, env, _| translate_expr_tail(st, env, else_e, result_cont.clone()))
}

fn translate_cond_expr_tail(st: &mut ProgSt, env: &Env,
  arms: &[(spiral::Expr, Vec<spiral::Stmt>)],
  result_cont: spine::ContName) -> Res<spine::Term>
{
  if arms.is_empty() {
    Ok(spine::Term::Cont(result_cont, vec![spine::Val::False]))
  } else {
    translate_branch_expr_tail(st, env, &arms[0].0,
      |st, env, _| {
        let (arm_onion, arm_res) = try!(translate_stmts(st, env, &arms[0].1[..]));
        Ok(arm_onion.subst_term(spine::Term::Cont(result_cont.clone(),
          vec![arm_res.into_val()])))
      },
      |st, env, _| translate_cond_expr_tail(st, env, &arms[1..], result_cont.clone()))
  }
}

fn translate_when_expr_tail(st: &mut ProgSt, env: &Env,
  cond: &spiral::Expr, body_stmts: &[spiral::Stmt],
  result_cont: spine::ContName) -> Res<spine::Term>
{
  translate_branch_expr_tail(st, env, cond,
    |st, env, _true_val| {
      let (body_onion, body_res) = try!(translate_stmts(st, env, body_stmts));
      Ok(body_onion.subst_term(spine::Term::Cont(result_cont.clone(), 
        vec![body_res.into_val()])))
    },
    |_st, _env, false_val| 
      Ok(spine::Term::Cont(result_cont.clone(), vec![false_val])))
}

fn translate_unless_expr_tail(st: &mut ProgSt, env: &Env,
  cond: &spiral::Expr, body_stmts: &[spiral::Stmt],
  result_cont: spine::ContName) -> Res<spine::Term>
{
  translate_branch_expr_tail(st, env, cond,
    |_st, _env, true_val| 
      Ok(spine::Term::Cont(result_cont.clone(), vec![true_val])),
    |st, env, _false_val| {
      let (body_onion, body_res) = try!(translate_stmts(st, env, body_stmts));
      Ok(body_onion.subst_term(spine::Term::Cont(result_cont.clone(), 
        vec![body_res.into_val()])))
    })
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

  let next_cont = st.gen_cont_name("do-next");
  let loop_cont = st.gen_cont_name("do-loop");
  let exit_cont = st.gen_cont_name("do-exit");

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
    Ok(spine::Term::Cont(result_cont, vec![spine::Val::True]))
  } else if exprs.len() == 1 {
    translate_expr_tail(st, env, &exprs[0], result_cont)
  } else {
    translate_branch_expr_tail(st, env, &exprs[0],
      |st, env, _true_val| translate_and_expr_tail(st, env, &exprs[1..],
        result_cont.clone()),
      |_st, _env, false_val| Ok(spine::Term::Cont(result_cont.clone(),
        vec![false_val])))
  }
}

fn translate_or_expr_tail(st: &mut ProgSt, env: &Env,
  exprs: &[spiral::Expr], result_cont: spine::ContName) -> Res<spine::Term>
{
  if exprs.len() == 0 {
    Ok(spine::Term::Cont(result_cont, vec![spine::Val::False]))
  } else if exprs.len() == 1 {
    translate_expr_tail(st, env, &exprs[0], result_cont)
  } else {
    translate_branch_expr_tail(st, env, &exprs[0],
      |_st, _env, true_val| Ok(spine::Term::Cont(result_cont.clone(),
        vec![true_val])),
      |st, env, _false_val| translate_or_expr_tail(st, env, &exprs[1..],
        result_cont.clone()))
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
  fun: &spiral::Expr, args: &[spiral::Expr],
  result_cont: spine::ContName) -> Res<spine::Term>
{
  let arg_refs: Vec<_> = args.iter().map(|arg| arg).collect();
  let (args_onion, arg_vals) = try!(translate_exprs(st, env, &arg_refs[..]));
  let (fun_onion, fun_val) = try!(translate_expr(st, env, fun));
  Ok(args_onion.subst_term(fun_onion.subst_term(
      spine::Term::Call(fun_val, result_cont, arg_vals))))
}

fn translate_lambda_expr(st: &mut ProgSt, env: &Env,
  args: &[spiral::Var], body_stmts: &[spiral::Stmt]) -> Res<(Onion, spine::Val)>
{
  let spine_var = st.gen_var("lambda");
  let spine_name = st.gen_fun_name("lambda");
  let clos_def = try!(translate_fun(st, env, spine_var.clone(), spine_name,
    args, body_stmts));
  Ok((Onion::Letclos(vec![clos_def], box Onion::Hole), spine::Val::Var(spine_var)))
}

fn bind_global_env(st: &mut ProgSt, parent: Env) -> Env {
  let extern_wrappers = &[
      ("println", "spiral_ext_println", vec!["x"]),
      ("+", "spiral_ext_add", vec!["a", "b"]),
      ("-", "spiral_ext_sub", vec!["a", "b"]),
      ("*", "spiral_ext_mul", vec!["a", "b"]),
      ("/", "spiral_ext_div", vec!["a", "b"]),
      ("<", "spiral_ext_lt",  vec!["a", "b"]),
      ("<=", "spiral_ext_le", vec!["a", "b"]),
      ("==", "spiral_ext_eq", vec!["a", "b"]),
      ("/=", "spiral_ext_ne", vec!["a", "b"]),
      (">", "spiral_ext_gt",  vec!["a", "b"]),
      (">=", "spiral_ext_ge", vec!["a", "b"]),

      ("vec-make", "spiral_ext_vec_make", vec!["len"]),
      ("vec-length", "spiral_ext_vec_length", vec!["vec"]),
      ("vec-get", "spiral_ext_vec_get", vec!["vec", "idx"]),
      ("vec-set!", "spiral_ext_vec_set", vec!["vec", "idx", "x"]),
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

#[cfg(test)]
mod test {
  use sexpr;
  use spine;
  use spiral;
  use spine::eval::{RtVal};
  use spine::eval::RtVal::{Int, True, False};

  fn run(txt: &str) -> Vec<RtVal> {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    let spiral = sexpr::to_spiral::prog_from_sexpr(&sexpr).unwrap();
    let spine = spiral::to_spine::spine_from_spiral(&spiral).unwrap();
    let errors = spine::check::check(&spine);
    if !errors.is_empty() {
      panic!("spine invalid: {:?}", errors)
    }
    let spine_sexpr = spine::to_sexpr::prog_to_sexpr(&spine);
    println!("{}", sexpr::pretty_print::pretty_print_sexpr(&spine_sexpr));
    spine::eval::eval(&spine)
  }

  #[test]
  fn test_empty_program() {
    assert_eq!(run("(program)"), vec![]);
  }

  #[test]
  fn test_output() {
    assert_eq!(run("(program (println 1) (println 2))"), vec![Int(1), Int(2)]);
  }

  #[test]
  fn test_if() {
    assert_eq!(run(
      "(program 
        (println (if false 10 20))
        (println (if true 10 20))
        (println (if 4 10 20)))"),
      vec![Int(20), Int(10), Int(10)]);
  }

  #[test]
  fn test_cond() {
    assert_eq!(run("(program (println (cond ((== 1 2) 99) ((< 1 2) 42) (1 66))))"),
      vec![Int(42)]);
    assert_eq!(run("(program (println (cond ((== 1 2) 99) ((> 1 2) 33) (true 42))))"),
      vec![Int(42)]);
  }

  #[test]
  fn test_when() {
    assert_eq!(run("(program (when (== 2 2) (println 4)) (when (== 1 2) (println 3)))"),
      vec![Int(4)]);
    assert_eq!(run("(program (when (== 4 4) (println 1) (println 2)) (println 4))"),
      vec![Int(1), Int(2), Int(4)]);
    assert_eq!(run("(program (when (< 4 4) (println 1) (println 2)) (println 4))"),
      vec![Int(4)]);
  }

  #[test]
  fn test_unless() {
    assert_eq!(run("(program (unless (== 2 2) (println 4)) (unless (== 3 2) (println 3)))"),
      vec![Int(3)]);
    assert_eq!(run("(program (unless (== 4 4) (println 1) (println 2)) (println 4))"),
      vec![Int(4)]);
    assert_eq!(run("(program (unless (< 4 4) (println 1) (println 2)) (println 4))"),
      vec![Int(1), Int(2), Int(4)]);
  }

  #[test]
  fn test_do() {
    assert_eq!(run(
      "(program (do ((f1 0 f2) (f2 1 (+ f1 f2)) (i 0 (+ i 1)))
                    ((>= i 10))
                  (println f1)))"),
      vec![Int(0), Int(1), Int(1), Int(2), Int(3),
        Int(5), Int(8), Int(13), Int(21), Int(34)]);
  }

  #[test]
  fn test_and() {
    assert_eq!(run(
        "(program
          (println (and 1 true 2))
          (println (and 3 4 false 5))
          (println (and)))"),
        vec![Int(2), False, True]);
  }

  #[test]
  fn test_or() {
    assert_eq!(run(
        "(program
          (println (or false 1 2))
          (println (or false false))
          (println (or)))"),
        vec![Int(1), False, False]);
  }

  #[test]
  fn test_begin() {
    assert_eq!(run("(program 
        (println 2)
        (begin
          (var x 1)
          (var y 3)
          (println (+ x y)))
        (println 8))"),
      vec![Int(2), Int(4), Int(8)]);
  }

  #[test]
  fn test_let() {
    assert_eq!(run("(program
        (println
          (let ((x 1) (y (+ x 1)) (z (+ y 1)) (w (+ z 1)))
              w)))"),
      vec![Int(4)]);
  }

  #[test]
  fn test_call() {
    assert_eq!(run("(program
        (fun square (x) (* x x))
        (fun neg (x) (- 0 x))
        (println (neg (square 2))))"),
      vec![Int(-4)]);
  }

  #[test]
  fn test_recursive_fun() {
    assert_eq!(run("(program
        (fun fac (x)
          (if (<= x 1) 1
            (* x (fac (- x 1)))))
        (println (fac 6))) "),
      vec![Int(720)]);
  }

  #[test]
  fn test_mutually_recursive_funs() {
    assert_eq!(run("(program
        (fun is-odd (n)
          (if (== n 0) false (is-even (- n 1))))
        (fun is-even (n)
          (if (== n 0) true (is-odd (- n 1))))
        (println (is-odd 6))
        (println (is-even 8))
        (println (is-odd 9)))"),
      vec![False, True, True]);
  }

  #[test]
  fn test_var_stmt() {
    assert_eq!(run("(program
      (fun return-42 () 42)
      (var forty-two (return-42))
      (println forty-two))"), vec![Int(42)]);
  }

  #[test]
  fn test_mutually_recursive_fun_in_var_stmt() {
    assert_eq!(run("(program
        (fun f-1 (x)
          (when (> x 1)
            (var y (f-2 (- x 1)))
            (println y)))
        (fun f-2 (x)
          (f-1 (- x 1))
          x)
        (f-1 4))"), vec![Int(1), Int(3)]);
  }
}
