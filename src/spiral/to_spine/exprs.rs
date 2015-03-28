use spiral;
use spine;
use spiral::to_spine::{ProgSt, Env, Res};
use spiral::to_spine::stmts::{translate_stmts};
use spine::onion::{Onion, OnionContDef};

pub fn translate_expr(st: &mut ProgSt, env: &Env, expr: &spiral::Expr)
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

pub fn translate_exprs(st: &mut ProgSt, env: &Env, exprs: &[&spiral::Expr])
  -> Res<(Onion, Vec<spine::Val>)>
{
  exprs.iter().fold(Ok((Onion::Hole, Vec::new())), |fold_val, expr| {
    let (onion, mut vals) = try!(fold_val);
    let (expr_onion, expr_val) = try!(translate_expr(st, env, expr));
    vals.push(expr_val);
    Ok((onion.subst_onion(expr_onion), vals))
  })
}

pub fn translate_expr_tail(st: &mut ProgSt, env: &Env,
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

pub fn translate_fun(st: &mut ProgSt, env: &Env,
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
