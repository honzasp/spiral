use std::collections::{HashSet};
use spiral;

pub fn collect_prog(prog: &spiral::Prog) -> HashSet<spiral::ModName> {
  let mut imported = HashSet::new();
  for stmt in prog.stmts.iter() {
    collect_stmt(&mut imported, stmt);
  }
  imported
}

pub fn collect_mod(module: &spiral::Mod) -> HashSet<spiral::ModName> {
  let mut imported = HashSet::new();
  for decl in module.decls.iter() {
    collect_decl(&mut imported, decl);
  }
  imported
}

fn collect_decl(imported: &mut HashSet<spiral::ModName>, decl: &spiral::Decl) {
  match *decl {
    spiral::Decl::Export(_) => (),
    spiral::Decl::Stmt(ref stmt) => collect_stmt(imported, stmt),
  }
}

fn collect_stmt(imported: &mut HashSet<spiral::ModName>, stmt: &spiral::Stmt) {
  match *stmt {
    spiral::Stmt::Import(ref import_defs) => 
      for import_def in import_defs.iter() {
        collect_import_def(imported, import_def)
      },
    spiral::Stmt::Fun(ref fun_def) =>
      for stmt in fun_def.stmts.iter() {
        collect_stmt(imported, stmt);
      },
    spiral::Stmt::Var(_, ref expr) => collect_expr(imported, expr),
    spiral::Stmt::Expr(ref expr) => collect_expr(imported, expr),
  }
}

fn collect_import_def(imported: &mut HashSet<spiral::ModName>, def: &spiral::ImportDef) {
  match *def {
    spiral::ImportDef::Mod(ref mod_name) => {
      imported.insert(mod_name.clone()); 
    },
    spiral::ImportDef::Only(ref sub_def, _) |
    spiral::ImportDef::Except(ref sub_def, _) |
    spiral::ImportDef::Prefix(ref sub_def, _) =>
      collect_import_def(imported, sub_def),
  }
}

fn collect_expr(imported: &mut HashSet<spiral::ModName>, expr: &spiral::Expr) {
  match *expr {
    spiral::Expr::If(ref cond_e, ref then_e, ref else_e) => {
      collect_expr(imported, cond_e);
      collect_expr(imported, then_e);
      collect_expr(imported, else_e);
    },
    spiral::Expr::Cond(ref arms) => 
      for &(ref arm_cond, ref arm_stmts) in arms.iter() {
        collect_expr(imported, arm_cond);
        for stmt in arm_stmts.iter() { collect_stmt(imported, stmt) }
      },
    spiral::Expr::When(ref expr, ref stmts) |
    spiral::Expr::Unless(ref expr, ref stmts)  => {
      collect_expr(imported, expr);
      for stmt in stmts.iter() { collect_stmt(imported, stmt) }
    },
    spiral::Expr::Do(ref vars, ref exit_cond, ref exit_stmts, ref body_stmts) => {
      for &(_, ref init_e, ref next_e) in vars.iter() {
        collect_expr(imported, init_e);
        collect_expr(imported, next_e);
      }
      collect_expr(imported, exit_cond);
      for stmt in exit_stmts.iter() { collect_stmt(imported, stmt) }
      for stmt in body_stmts.iter() { collect_stmt(imported, stmt) }
    },
    spiral::Expr::Extern(_, ref exprs) |
    spiral::Expr::And(ref exprs) |
    spiral::Expr::Or(ref exprs) =>
      for expr in exprs.iter() { collect_expr(imported, expr) },
    spiral::Expr::Lambda(_, ref stmts) |
    spiral::Expr::Begin(ref stmts) =>
      for stmt in stmts.iter() { collect_stmt(imported, stmt) },
    spiral::Expr::Let(ref arms, ref stmts) => {
      for &(_, ref expr) in arms.iter() { collect_expr(imported, expr) }
      for stmt in stmts.iter() { collect_stmt(imported, stmt) }
    },
    spiral::Expr::Call(ref fun_e, ref args) => {
      collect_expr(imported, fun_e);
      for arg in args.iter() { collect_expr(imported, arg) }
    },
    spiral::Expr::Var(_) |
    spiral::Expr::String(_) |
    spiral::Expr::Int(_) => (),
  }
}
