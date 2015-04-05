use std::borrow::{Borrow};
use std::collections::{HashSet};
use spiral;
use spine;
use spiral::to_spine::{ProgSt, Env, Res};
use spiral::to_spine::exprs::{translate_fun, translate_expr, translate_expr_tail};
use spine::onion::{Onion};

pub fn translate_stmts_tail<S: Borrow<spiral::Stmt>>(st: &mut ProgSt, env: &Env,
  stmts: &[S], result_cont: spine::ContName) -> Res<spine::Term>
{
  if stmts.len() == 0 {
    Ok(spine::Term::Cont(result_cont, vec![spine::Val::False]))
  } else if stmts.len() == 1 {
    translate_stmt_tail(st, env, stmts[0].borrow(), result_cont)
  } else {
    let ((onion, inner_env), nexts) = try!(translate_stmt_block(st, env, stmts));
    Ok(onion.subst_term(try!(translate_stmts_tail(
      st, &inner_env, nexts, result_cont))))
  }
}

pub fn translate_stmts<S: Borrow<spiral::Stmt>>(st: &mut ProgSt, env: &Env,
  stmts: &[S]) -> Res<(Onion, Env)>
{
  if stmts.len() == 0 {
    Ok((Onion::Hole, env.clone()))
  } else {
    let ((outer_onion, outer_env), nexts) = try!(translate_stmt_block(st, env, stmts));
    let (inner_onion, inner_env) = try!(translate_stmts(st, &outer_env, nexts));
    Ok((outer_onion.subst_onion(inner_onion), inner_env))
  }
}

fn translate_stmt_block<'a, S: Borrow<spiral::Stmt>>(st: &mut ProgSt, env: &Env,
  stmts: &'a [S]) -> Res<((Onion, Env), &'a [S])>
{
  let fun_defs: Vec<_> = stmts.iter()
    .take_while(|&stmt| match *stmt.borrow() {
      spiral::Stmt::Fun(_) => true,
      spiral::Stmt::Var(_, _) | spiral::Stmt::Expr(_) => false,
      spiral::Stmt::Import(_) => false,
    }).map(|stmt| match *stmt.borrow() {
      spiral::Stmt::Fun(ref fun_def) => fun_def,
      _ => panic!("non-fun stmts should have been filtered"),
    }).collect();

  if !fun_defs.is_empty() {
    Ok((try!(translate_fun_stmts(st, env, &fun_defs[..])), &stmts[fun_defs.len()..]))
  } else {
    Ok((try!(translate_stmt(st, env, stmts[0].borrow())), &stmts[1..]))
  }
}

fn translate_fun_stmts(st: &mut ProgSt, env: &Env, fun_defs: &[&spiral::FunDef])
  -> Res<(Onion, Env)>
{
  let spine_vars: Vec<_> = fun_defs.iter().map(|fun_def| {
      st.gen_var(&fun_def.var.0[..])
    }).collect();

  let inner_env = env.bind_vars(fun_defs.iter().zip(spine_vars.iter())
      .map(|(fun_def, spine_var)| {
        (fun_def.var.clone(), spine::Val::Var(spine_var.clone()))
      }).collect());

  let mut spine_defs = Vec::new();
  for (fun_def, spine_var) in fun_defs.iter().zip(spine_vars.into_iter()) {
    spine_defs.push(try!(translate_fun(st, &inner_env, spine_var, 
        &fun_def.args[..], &fun_def.stmts[..])));
  }

  Ok((Onion::Letfun(spine_defs, box Onion::Hole), inner_env))
}

fn translate_stmt_tail(st: &mut ProgSt, env: &Env, stmt: &spiral::Stmt,
  result_cont: spine::ContName) -> Res<spine::Term>
{
  match *stmt {
    spiral::Stmt::Expr(ref expr) =>
      translate_expr_tail(st, env, expr, result_cont),
    _ => {
      let (stmt_onion, _) = try!(translate_stmt(st, env, stmt));
      Ok(stmt_onion.subst_term(spine::Term::Cont(result_cont, vec![spine::Val::False])))
    }
  }
}


fn translate_stmt(st: &mut ProgSt, env: &Env, stmt: &spiral::Stmt) 
  -> Res<(Onion, Env)>
{
  match *stmt {
    spiral::Stmt::Import(ref import_defs) => {
      let mut import_env = env.clone();
      for import_def in import_defs.iter() {
        import_env = import_env.bind_vars(try!(translate_import_def(env, import_def)));
      }
      Ok((Onion::Hole, import_env))
    },
    spiral::Stmt::Fun(ref fun_def) =>
      translate_fun_stmts(st, env, &[fun_def]),
    spiral::Stmt::Var(ref var, ref expr) => {
      let (expr_onion, expr_val) = try!(translate_expr(st, env, expr));
      let inner_env = env.bind_vars(vec![(var.clone(), expr_val)]);
      Ok((expr_onion, inner_env))
    },
    spiral::Stmt::Expr(ref expr) => {
      let (expr_onion, _expr_val) = try!(translate_expr(st, env, expr));
      Ok((expr_onion, env.clone()))
    },
  }
}

fn translate_import_def(env: &Env, import_def: &spiral::ImportDef)
  -> Res<Vec<(spiral::Var, spine::Val)>>
{
  match *import_def {
    spiral::ImportDef::Mod(ref mod_name) => 
      match env.lookup_mod(mod_name) {
        Some(exports) => Ok(exports.clone()),
        None => Err(format!("undefined mod '{}'", mod_name.0)),
      },
    spiral::ImportDef::Only(ref def, ref only_vars) => 
      filtered_import_def(env, def, only_vars, false),
    spiral::ImportDef::Except(ref def, ref except_vars) => 
      filtered_import_def(env, def, except_vars, true),
    spiral::ImportDef::Prefix(ref def, ref prefix) =>
      Ok(try!(translate_import_def(env, def)).into_iter().map(|(var, val)| {
        (spiral::Var(prefix.0.clone() + &var.0[..]), val)
      }).collect()),
  }
}

fn filtered_import_def(env: &Env, import_def: &spiral::ImportDef,
  vars: &[spiral::Var], include: bool) 
  -> Res<Vec<(spiral::Var, spine::Val)>>
{
  let mut var_set: HashSet<_> = vars.iter().collect();
  let selected = try!(translate_import_def(env, import_def)).into_iter()
      .filter(|&(ref var, _)| { var_set.remove(var) ^ include }).collect();
  if !var_set.is_empty() {
    Err(format!("names {:?} in '{}' import were missing", var_set,
      if include { "only" } else { "except" }))
  } else {
    Ok(selected)
  }
}
