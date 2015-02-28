use std::collections::{HashSet};
use spine;

#[derive(PartialEq, Debug)]
pub struct ContDef {
  pub name: spine::ContName,
  pub args: Vec<spine::Var>,
  pub body: Term,
  pub free_args: HashSet<spine::Var>,
  pub free_vars: HashSet<spine::Var>,
}

#[derive(PartialEq, Debug)]
pub enum Term {
  Letcont(Vec<ContDef>, Box<Term>),
  Call(spine::FunName, spine::ContName, Vec<spine::Val>),
  ExternCall(spine::ExternName, spine::ContName, Vec<spine::Val>),
  Cont(spine::ContName, Vec<spine::Val>),
  Branch(spine::Boolval, spine::ContName, spine::ContName),
}

pub fn census_from_term(term: &spine::Term) -> (Term, HashSet<spine::Var>) {
  let mut census = HashSet::new();
  (walk_term(&mut census, term), census)
}

fn walk_term(census: &mut HashSet<spine::Var>, term: &spine::Term) -> Term {
  match *term {
    spine::Term::Letcont(ref cont_defs, ref body) => {
      let census_defs = cont_defs.iter().map(|cont_def| {
        let mut cont_census = HashSet::new();
        let cont_body_term = walk_term(&mut cont_census, &cont_def.body);
        let cont_free_args = cont_def.args.iter()
            .filter(|arg| cont_census.remove(arg)).cloned().collect();
        census.extend(cont_census.iter().cloned());

        ContDef {
          name: cont_def.name.clone(),
          args: cont_def.args.clone(),
          body: cont_body_term,
          free_args: cont_free_args,
          free_vars: cont_census,
        }
      }).collect();
      Term::Letcont(census_defs, box walk_term(census, body))
    },
    spine::Term::Call(ref fun_name, ref ret_cont, ref args) =>
      Term::Call(fun_name.clone(), ret_cont.clone(),
        args.iter().map(|arg| walk_val(census, arg)).collect()),
    spine::Term::ExternCall(ref extern_name, ref ret_cont, ref args) =>
      Term::ExternCall(extern_name.clone(), ret_cont.clone(),
        args.iter().map(|arg| walk_val(census, arg)).collect()),
    spine::Term::Cont(ref cont, ref args) =>
      Term::Cont(cont.clone(), 
        args.iter().map(|arg| walk_val(census, arg)).collect()),
    spine::Term::Branch(ref boolval, ref then_cont, ref else_cont) =>
      Term::Branch(walk_boolval(census, boolval), then_cont.clone(), else_cont.clone()),
  }
}

fn walk_val(census: &mut HashSet<spine::Var>, val: &spine::Val) -> spine::Val {
  match *val {
    spine::Val::Literal(num) => { },
    spine::Val::Var(ref var) => { census.insert(var.clone()); },
  }
  val.clone()
}

fn walk_boolval(census: &mut HashSet<spine::Var>, boolval: &spine::Boolval) 
  -> spine::Boolval
{
  match *boolval {
    spine::Boolval::IsTrue(ref val) => spine::Boolval::IsTrue(walk_val(census, val)),
    spine::Boolval::IsFalse(ref val) => spine::Boolval::IsFalse(walk_val(census, val)),
  }
}
