use std::collections::{HashSet};
use spine;

pub fn collect_term(term: &spine::Term) -> HashSet<spine::Var> {
  let mut free = HashSet::new();
  match *term {
    spine::Term::Letcont(ref cont_defs, ref body) => {
      free = collect_term(&**body);
      for cont_def in cont_defs.iter() {
        free.extend(collect_cont_def(cont_def).into_iter());
      }
    },
    spine::Term::Letclos(ref clos_defs, ref body) => {
      free = collect_term(&**body);
      for clos_def in clos_defs.iter() {
        free.extend(collect_closure_def(clos_def).into_iter());
      }
      for clos_def in clos_defs.iter() {
        free.remove(&clos_def.var);
      }
    },
    spine::Term::Call(ref fun, _, ref args) => {
      free = collect_val(fun).into_iter().collect();
      for arg in args.iter() {
        free.extend(collect_val(arg).into_iter());
      }
    },
    spine::Term::ExternCall(_, _, ref args) |
    spine::Term::Cont(_, ref args) => {
      for arg in args.iter() {
        free.extend(collect_val(arg).into_iter());
      }
    },
    spine::Term::Branch(ref boolval, _, _) =>
      free = collect_boolval(boolval).into_iter().collect(),
  }
  free
}

pub fn collect_cont_def(cont_def: &spine::ContDef) -> HashSet<spine::Var> {
  let mut free = collect_term(&cont_def.body);
  for arg in cont_def.args.iter() {
    free.remove(arg);
  }
  free
}

pub fn collect_closure_def(clos_def: &spine::ClosureDef) -> HashSet<spine::Var> {
  let mut free = HashSet::new();
  for capture in clos_def.captures.iter() {
    free.extend(collect_val(capture).into_iter());
  }
  free
}

fn collect_val(val: &spine::Val) -> Option<spine::Var> {
  match *val {
    spine::Val::Var(ref var) =>
      Some(var.clone()),
    spine::Val::Combinator(_) |
    spine::Val::Int(_) |
    spine::Val::Obj(_) |
    spine::Val::True |
    spine::Val::False =>
      None,
  }
}

fn collect_boolval(boolval: &spine::Boolval) -> Option<spine::Var> {
  match *boolval {
    spine::Boolval::IsTrue(ref val) => collect_val(val),
    spine::Boolval::IsFalse(ref val) => collect_val(val),
  }
}

#[cfg(test)]
mod test {
  use std::collections::{HashSet};
  use sexpr;
  use spine;

  fn parse_collect(txt: &str) -> HashSet<spine::Var> {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    let spine = sexpr::to_spine::term_from_sexpr(&sexpr).unwrap();
    spine::free::collect_term(&spine)
  }

  fn var_set(vars: Vec<&str>) -> HashSet<spine::Var> {
    vars.iter().map(|v| spine::Var(v.to_string())).collect()
  }

  #[test]
  fn test_collect_simple_term() {
    assert_eq!(parse_collect("(call (combinator Y) k a 10 b)"),
      var_set(vec!["a", "b"]));
    assert_eq!(parse_collect("(call f k a b)"),
      var_set(vec!["f", "a", "b"]));
    assert_eq!(parse_collect("(extern-call panic k a b c)"),
      var_set(vec!["a", "b", "c"]));
    assert_eq!(parse_collect("(cont cc a b 3)"),
      var_set(vec!["a", "b"]));
    assert_eq!(parse_collect("(branch (is-true x) cc1 cc2)"),
      var_set(vec!["x"]));
    assert_eq!(parse_collect("(branch (is-false x) cc1 cc2)"),
      var_set(vec!["x"]));
  }

  #[test]
  fn test_collect_letclos() {
    assert_eq!(parse_collect(
      "(letclos
        ((clos-1 fun-1 a b clos-2)
         (clos-2 fun-2 clos-1 c)
         (clos-3 fun-3 d e)
         (clos-4 fun-4 clos-3 clos-2 clos-4 f))
        (cont cc clos-1 clos-2))"),
      var_set(vec!["a", "b", "c", "d", "e", "f"]));
  }

  #[test]
  fn test_collect_letcont() {
    assert_eq!(parse_collect(
        "(letcont
          ((cc1 (a b) (cont cc a b x))
           (cc2 (c) (cont cc a))
           (cc3 (b e) 
            (letcont ((cc4 (z) (cont cc b z w)))
              (cont cc b))))
          (cont cc d e))"),
        var_set(vec!["x", "a", "w", "d", "e"]));
  }
}
