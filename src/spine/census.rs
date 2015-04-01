#![allow(dead_code)]
use std::collections::{HashSet};
use spine;

#[derive(PartialEq, Debug)]
pub struct ContDef {
  pub name: spine::ContName,
  pub args: Vec<spine::Var>,
  pub body: Term,
  pub free_args: HashSet<spine::Var>,
  pub free_vars: HashSet<spine::Var>,
  pub free_conts: HashSet<spine::ContName>,
}

#[derive(PartialEq, Debug)]
pub enum Term {
  Letcont(Vec<ContDef>, Box<Term>),
  Letclos(Vec<spine::ClosureDef>, Box<Term>),
  Call(spine::Val, spine::ContName, Vec<spine::Val>),
  ExternCall(spine::ExternName, spine::ContName, Vec<spine::Val>),
  Cont(spine::ContName, Vec<spine::Val>),
  Branch(spine::Boolval, spine::ContName, spine::ContName),
}

#[derive(Debug)]
pub struct Census {
  vars: HashSet<spine::Var>,
  conts: HashSet<spine::ContName>,
}

pub fn census_from_term(term: &spine::Term) -> (Term, Census) {
  let mut census = Census {
    vars: HashSet::new(),
    conts: HashSet::new(),
  };

  (walk_term(&mut census, term), census)
}

fn walk_term(census: &mut Census, term: &spine::Term) -> Term {
  match *term {
    spine::Term::Letcont(ref cont_defs, ref body) => {
      let census_defs = cont_defs.iter().map(|cont_def| {
        let mut cont_census = Census {
          vars: HashSet::new(),
          conts: HashSet::new(),
        };

        let cont_body_term = walk_term(&mut cont_census, &cont_def.body);
        let cont_free_args = cont_def.args.iter()
            .filter(|arg| cont_census.vars.remove(arg)).cloned().collect();
        census.vars.extend(cont_census.vars.iter().cloned());
        census.conts.extend(cont_census.conts.iter().cloned());

        ContDef {
          name: cont_def.name.clone(),
          args: cont_def.args.clone(),
          body: cont_body_term,
          free_args: cont_free_args,
          free_vars: cont_census.vars,
          free_conts: cont_census.conts,
        }
      }).collect();
      let walked_body = walk_term(census, body);
      for cont_def in cont_defs.iter() {
        census.conts.remove(&cont_def.name);
      }
      Term::Letcont(census_defs, box walked_body)
    },
    spine::Term::Letclos(ref clos_defs, ref body) => {
      let mut body_census = Census {
        vars: HashSet::new(),
        conts: HashSet::new(),
      };
      let body_term = walk_term(&mut body_census, &**body);

      for clos_def in clos_defs.iter() {
        for capture in clos_def.captures.iter() {
          walk_val(&mut body_census, capture);
        }
      }

      for clos_def in clos_defs.iter() {
        body_census.vars.remove(&clos_def.var);
      }
      census.vars.extend(body_census.vars.into_iter());
      census.conts.extend(body_census.conts.into_iter());
      Term::Letclos(clos_defs.clone(), box body_term)
    },
    spine::Term::Call(ref fun, ref ret_cont, ref args) => {
      census.conts.insert(ret_cont.clone());
      Term::Call(fun.clone(), ret_cont.clone(),
        args.iter().map(|arg| walk_val(census, arg)).collect())
    },
    spine::Term::ExternCall(ref extern_name, ref ret_cont, ref args) => {
      census.conts.insert(ret_cont.clone());
      Term::ExternCall(extern_name.clone(), ret_cont.clone(),
        args.iter().map(|arg| walk_val(census, arg)).collect())
    },
    spine::Term::Cont(ref cont, ref args) => {
      census.conts.insert(cont.clone());
      Term::Cont(cont.clone(), 
        args.iter().map(|arg| walk_val(census, arg)).collect())
    },
    spine::Term::Branch(ref boolval, ref then_cont, ref else_cont) => {
      census.conts.insert(then_cont.clone());
      census.conts.insert(else_cont.clone());
      Term::Branch(walk_boolval(census, boolval), then_cont.clone(), else_cont.clone())
    },
  }
}

fn walk_val(census: &mut Census, val: &spine::Val) -> spine::Val {
  match *val {
    spine::Val::Obj(_) | spine::Val::Int(_) => (),
    spine::Val::True | spine::Val::False => (),
    spine::Val::Combinator(_) => (),
    spine::Val::Var(ref var) => { census.vars.insert(var.clone()); },
  }
  val.clone()
}

fn walk_boolval(census: &mut Census, boolval: &spine::Boolval) 
  -> spine::Boolval
{
  match *boolval {
    spine::Boolval::IsTrue(ref val) => spine::Boolval::IsTrue(walk_val(census, val)),
    spine::Boolval::IsFalse(ref val) => spine::Boolval::IsFalse(walk_val(census, val)),
  }
}
