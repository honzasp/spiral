use spine;
use sexpr;

pub fn prog_to_sexpr(prog: &spine::ProgDef) -> sexpr::Elem {
  sexpr::Elem::List(vec![
    ident("program"),
    cont_name_to_sexpr(&prog.halt_cont),
    term_to_sexpr(&prog.body),
  ])
}

pub fn term_to_sexpr(term: &spine::Term) -> sexpr::Elem {
  let mut list = vec![];
  match *term {
    spine::Term::Letcont(ref cont_defs, ref body) => {
      list.push(ident("letcont"));
      list.extend(cont_defs.iter().map(cont_def_to_sexpr));
      list.push(term_to_sexpr(&**body));
    },
    spine::Term::Letfun(ref fun_defs, ref body) => {
      list.push(ident("letfun"));
      list.extend(fun_defs.iter().map(fun_def_to_sexpr));
      list.push(term_to_sexpr(&**body));
    },
    spine::Term::Letobj(ref obj_def, ref body) => {
      list.push(ident("letobj"));
      list.push(obj_def_to_sexpr(obj_def));
      list.push(term_to_sexpr(&**body));
    },
    spine::Term::Call(ref fun, ref ret, ref args) => {
      list.push(ident("call"));
      list.push(val_to_sexpr(fun));
      list.push(cont_name_to_sexpr(ret));
      list.extend(args.iter().map(val_to_sexpr));
    },
    spine::Term::ExternCall(ref ext_fun, ref ret, ref args) => {
      list.push(ident("extern-call"));
      list.push(extern_name_to_sexpr(ext_fun));
      list.push(cont_name_to_sexpr(ret));
      list.extend(args.iter().map(val_to_sexpr));
    },
    spine::Term::Cont(ref cont, ref args) => {
      list.push(ident("cont"));
      list.push(cont_name_to_sexpr(cont));
      list.extend(args.iter().map(val_to_sexpr));
    },
    spine::Term::Branch(ref boolval, ref then_cont, ref else_cont) => {
      list.push(ident("branch"));
      list.push(boolval_to_sexpr(boolval));
      list.push(cont_name_to_sexpr(then_cont));
      list.push(cont_name_to_sexpr(else_cont));
    },
  }
  sexpr::Elem::List(list)
}

pub fn fun_def_to_sexpr(def: &spine::FunDef) -> sexpr::Elem {
  sexpr::Elem::List(vec![
    var_to_sexpr(&def.var),
    cont_name_to_sexpr(&def.ret),
    sexpr::Elem::List(def.captures.iter().map(var_to_sexpr).collect()),
    sexpr::Elem::List(def.args.iter().map(var_to_sexpr).collect()),
    term_to_sexpr(&def.body),
  ])
}

pub fn obj_def_to_sexpr(def: &spine::ObjDef) -> sexpr::Elem {
  use std::{str};
  sexpr::Elem::List(match def.obj {
    spine::Obj::String(ref bytes) => {
      let header = vec![ident("string"), var_to_sexpr(&def.var)];
      let body = match str::from_utf8(&bytes[..]) {
        Ok(txt) => vec![sexpr::Elem::String(txt.to_string())],
        Err(_) => bytes.iter().map(|&b| sexpr::Elem::Int(b as i32)).collect(),
      };
      header.into_iter().chain(body.into_iter()).collect()
    },
    spine::Obj::Double(number) => 
      vec![ident("double"), var_to_sexpr(&def.var), sexpr::Elem::Double(number)],
  })
}

pub fn cont_def_to_sexpr(cont_def: &spine::ContDef) -> sexpr::Elem {
  sexpr::Elem::List(vec![
    cont_name_to_sexpr(&cont_def.name),
    sexpr::Elem::List(cont_def.args.iter().map(var_to_sexpr).collect()),
    term_to_sexpr(&cont_def.body),
  ])
}

pub fn val_to_sexpr(val: &spine::Val) -> sexpr::Elem {
  match *val {
    spine::Val::Var(ref var) => ident(&var.0[..]),
    spine::Val::Int(num) => sexpr::Elem::Int(num),
    spine::Val::True =>
      sexpr::Elem::List(vec![ident("true")]),
    spine::Val::False => 
      sexpr::Elem::List(vec![ident("false")]),
  }
}

pub fn boolval_to_sexpr(boolval: &spine::Boolval) -> sexpr::Elem {
  sexpr::Elem::List(match *boolval {
    spine::Boolval::IsTrue(ref val) => vec![ident("is-true"), val_to_sexpr(val)],
    spine::Boolval::IsFalse(ref val) => vec![ident("is-false"), val_to_sexpr(val)],
  })
}

pub fn var_to_sexpr(i: &spine::Var) -> sexpr::Elem { ident(&i.0[..]) }
pub fn extern_name_to_sexpr(i: &spine::ExternName) -> sexpr::Elem { ident(&i.0[..]) }
pub fn cont_name_to_sexpr(i: &spine::ContName) -> sexpr::Elem { ident(&i.0[..]) }

fn ident(id: &str) -> sexpr::Elem {
  sexpr::Elem::Identifier(id.to_string())
}

#[cfg(test)]
mod test {
  use spine;
  use sexpr;

  fn check_identity(txt: &str) {
    let sexpr_1 = sexpr::parse::parse_sexpr(txt).unwrap();
    let spine_1 = sexpr::to_spine::prog_from_sexpr(&sexpr_1).unwrap();
    let sexpr_2 = spine::to_sexpr::prog_to_sexpr(&spine_1);
    assert_eq!(sexpr_1, sexpr_2);
  }

  #[test]
  fn test_letfun() {
    check_identity("(program halt
      (letfun (fun1 r (x y) (a b) (cont r a))
              (fun2 q () (a) (cont r 10))
        (cont halt)))")
  }

  #[test]
  fn test_letobj() {
    check_identity("(program halt
      (letobj (string utf8 \"a utf-8 string\")
        (letobj (string non-utf8 0 255 254)
          (cont halt))))")
  }

  #[test]
  fn test_terms() {
    check_identity("(program halt
      (letfun (fun1 r (c1 c2) (a) (cont r 10))
              (fun2 q () (a b c) (cont q 20))
        (letcont (cc1 (a b) (call some-fun return-cont a 10))
                 (cc2 (x y z) (cont cc-x foo 10 bar))
                 (cc3 () (extern-call drop-the-bomb cc 1 2 3))
                 (cc4 () (branch (is-false 100) not-ok ok))
                 (cc5 () (branch (is-true 100) ok not-ok))
          (cont halt))))")
  }

  #[test]
  fn test_vals() {
    check_identity("(program halt
      (letcont (cc-vars () (cont cc x y z))
               (cc-bools () (cont cc (true) (false)))
               (cc-ints () (cont cc 1 2 3))
        (cont halt)))")
  }
}
