use std::collections::{HashSet};
use spiral;
use spine;
use spiral::to_spine::{ProgSt, Env, Res};
use spiral::to_spine::exprs::{translate_fun, translate_expr};
use spine::onion::{Onion};

#[derive(Debug, Clone)]
pub enum StmtRes {
  Env(Env),
  Val(spine::Val),
  Empty,
}

impl StmtRes {
  pub fn into_val(self) -> spine::Val {
    match self {
      StmtRes::Val(val) => val,
      StmtRes::Env(_) | StmtRes::Empty => spine::Val::False,
    }
  }
}

pub fn translate_stmts(st: &mut ProgSt, env: &Env, stmts: &[spiral::Stmt])
  -> Res<(Onion, StmtRes)>
{
  translate_stmts_refs(st, env,
    &stmts.iter().collect::<Vec<_>>()[..])
}

pub fn translate_stmts_refs(st: &mut ProgSt, env: &Env, stmts: &[&spiral::Stmt])
  -> Res<(Onion, StmtRes)>
{
  if stmts.len() == 0 {
    Ok((Onion::Hole, StmtRes::Env(env.clone())))
  } else if stmts.len() == 1 {
    translate_stmt(st, env, stmts[0])
  } else {
    let fun_defs: Vec<_> = stmts.iter()
      .take_while(|&&stmt| match *stmt {
        spiral::Stmt::Fun(_) => true,
        spiral::Stmt::Var(_, _) | spiral::Stmt::Expr(_) => false,
        spiral::Stmt::Import(_) => false,
      }).map(|&stmt| match *stmt {
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
      StmtRes::Env(ref inner_env) => translate_stmts_refs(st, inner_env, next_stmts),
      StmtRes::Val(_) | StmtRes::Empty => translate_stmts_refs(st, env, next_stmts),
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
        &fun_def.args[..], &fun_def.stmts[..])));
  }

  Ok((Onion::Letclos(clos_defs, box Onion::Hole), StmtRes::Env(inner_env)))
}

fn translate_stmt(st: &mut ProgSt, env: &Env, stmt: &spiral::Stmt)
  -> Res<(Onion, StmtRes)>
{
  match *stmt {
    spiral::Stmt::Import(ref import_defs) => {
      let mut import_env = env.clone();
      for import_def in import_defs.iter() {
        import_env = import_env.bind_vars(try!(translate_import_def(env, import_def)));
      }
      Ok((Onion::Hole, StmtRes::Env(import_env)))
    },
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
