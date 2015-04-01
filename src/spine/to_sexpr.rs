use spine;
use sexpr;

pub fn prog_to_sexpr(prog: &spine::ProgDef) -> sexpr::Elem {
  let header = vec![ident("program"), fun_name_to_sexpr(&prog.main_fun)].into_iter();
  let funs = prog.fun_defs.iter().map(fun_def_to_sexpr);
  let objs = prog.obj_defs.iter().map(obj_def_to_sexpr);
  sexpr::Elem::List(header.chain(funs).chain(objs).collect())
}

pub fn fun_def_to_sexpr(def: &spine::FunDef) -> sexpr::Elem {
  sexpr::Elem::List(vec![
    ident("fun"),
    fun_name_to_sexpr(&def.name),
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
      let header = vec![ident("string"), obj_name_to_sexpr(&def.name)];
      let body = match str::from_utf8(&bytes[..]) {
        Ok(txt) => vec![sexpr::Elem::String(txt.to_string())],
        Err(_) => bytes.iter().map(|&b| sexpr::Elem::Int(b as i32)).collect(),
      };
      header.into_iter().chain(body.into_iter()).collect()
    },
  })
}

pub fn term_to_sexpr(term: &spine::Term) -> sexpr::Elem {
  sexpr::Elem::List(match *term {
    spine::Term::Letcont(ref cont_defs, ref body) => vec![
        ident("letcont"),
        sexpr::Elem::List(cont_defs.iter().map(cont_def_to_sexpr).collect()),
        term_to_sexpr(&**body),
      ],
    spine::Term::Letclos(ref clos_defs, ref body) => vec![
        ident("letclos"),
        sexpr::Elem::List(clos_defs.iter().map(clos_def_to_sexpr).collect()),
        term_to_sexpr(&**body),
      ],
    spine::Term::Call(ref fun, ref ret, ref args) => vec_plus(vec![
        ident("call"),
        val_to_sexpr(fun),
        cont_name_to_sexpr(ret),
      ], args.iter().map(val_to_sexpr).collect()),
    spine::Term::ExternCall(ref ext_fun, ref ret, ref args) => vec_plus(vec![
        ident("extern-call"),
        extern_name_to_sexpr(ext_fun),
        cont_name_to_sexpr(ret),
      ], args.iter().map(val_to_sexpr).collect()),
    spine::Term::Cont(ref cont, ref args) => vec_plus(vec![
        ident("cont"),
        cont_name_to_sexpr(cont),
      ], args.iter().map(val_to_sexpr).collect()),
    spine::Term::Branch(ref boolval, ref then_cont, ref else_cont) => vec![
        ident("branch"),
        boolval_to_sexpr(boolval),
        cont_name_to_sexpr(then_cont),
        cont_name_to_sexpr(else_cont),
      ],
  })
}

pub fn cont_def_to_sexpr(cont_def: &spine::ContDef) -> sexpr::Elem {
  sexpr::Elem::List(vec![
    cont_name_to_sexpr(&cont_def.name),
    sexpr::Elem::List(cont_def.args.iter().map(var_to_sexpr).collect()),
    term_to_sexpr(&cont_def.body),
  ])
}

pub fn clos_def_to_sexpr(clos_def: &spine::ClosureDef) -> sexpr::Elem {
  sexpr::Elem::List(vec_plus(vec![
      var_to_sexpr(&clos_def.var),
      fun_name_to_sexpr(&clos_def.fun_name),
    ], clos_def.captures.iter().map(val_to_sexpr).collect()))
}


pub fn val_to_sexpr(val: &spine::Val) -> sexpr::Elem {
  match *val {
    spine::Val::Var(ref var) => ident(&var.0[..]),
    spine::Val::Int(num) => sexpr::Elem::Int(num),
    spine::Val::Combinator(ref fun_name) =>
      sexpr::Elem::List(vec![ident("combinator"), fun_name_to_sexpr(fun_name)]),
    spine::Val::Obj(ref obj_name) =>
      sexpr::Elem::List(vec![ident("obj"), obj_name_to_sexpr(obj_name)]),
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
pub fn fun_name_to_sexpr(i: &spine::FunName) -> sexpr::Elem { ident(&i.0[..]) }
pub fn extern_name_to_sexpr(i: &spine::ExternName) -> sexpr::Elem { ident(&i.0[..]) }
pub fn cont_name_to_sexpr(i: &spine::ContName) -> sexpr::Elem { ident(&i.0[..]) }
pub fn obj_name_to_sexpr(i: &spine::ObjName) -> sexpr::Elem { ident(&i.0[..]) }

fn ident(id: &str) -> sexpr::Elem {
  sexpr::Elem::Identifier(id.to_string())
}

fn vec_plus<T>(mut xs: Vec<T>, ys: Vec<T>) -> Vec<T> {
  xs.extend(ys.into_iter());
  xs
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
  fn test_empty_prog() {
    check_identity("(program start)")
  }

  #[test]
  fn test_prog_with_fun() {
    check_identity("(program start (fun start r (a b) (x y z) (cont r)))")
  }

  #[test]
  fn test_prog_with_obj() {
    check_identity("(program start (string rust \"rust\"))")
  }

  #[test]
  fn test_string_objs() {
    check_identity("(program start
      (string utf8 \"a utf-8 string\")
      (string non-utf8 0 255 254))")
  }

  #[test]
  fn test_terms() {
    check_identity("(program start
      (fun start r () () 
        (letclos ((clos1 fun1 10 20)
                  (clos2 fun2 30))
          (letcont ((cc1 (a b) (call some-fun return-cont a 10))
                    (cc2 (x y z) (cont cc-x foo 10 bar))
                    (cc3 () (extern-call drop-the-bomb cc 1 2 3))
                    (cc4 () (branch (is-false 100) not-ok ok)))
            (cont halt)))))")
  }

  #[test]
  fn test_vals() {
    check_identity("(program start
      (fun start r () ()
        (letcont ((cc-vars () (cont cc x y z))
                  (cc-bools () (cont cc (true) (false)))
                  (cc-ints () (cont cc 1 2 3))
                  (cc-objs () (cont cc (obj a) (obj b)))
                  (cc-combinators () (cont cc (combinator ff) (combinator Y))))
          (cont halt))))")
  }
}
