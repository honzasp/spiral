use sexpr;
use spine;

pub fn prog_from_sexpr(prog: &sexpr::Elem) -> Result<spine::ProgDef, String> {
  match *prog {
    sexpr::Elem::List(ref list) => {
      match list.get(0) {
        Some(&sexpr::Elem::Identifier(ref head)) if head.as_slice() == "program" => (),
        _ => return Err(format!("program has to begin with 'program'")),
      };

      let main_fun = try!(fun_name_from_sexpr(try!(list.get(1)
        .ok_or_else(|| format!("program has to specify main fun")))));

      let mut fun_defs = Vec::new();
      let mut obj_defs = Vec::new();
      for def in list[2..].iter() {
        match *def {
          sexpr::Elem::List(ref def_list) => 
            match def_list.get(0) {
              Some(&sexpr::Elem::Identifier(ref def_head)) => match &def_head[..] {
                "fun" => fun_defs.push(try!(fun_def_from_sexprs(&def_list[1..]))),
                "string" => obj_defs.push(try!(str_def_from_sexprs(&def_list[1..]))),
                _ => return Err(format!("invalid def head")),
              },
              _ => return Err(format!("def must begin with an identifier")),
            },
          _ => return Err(format!("def must be a list")),
        }
      }

      Ok(spine::ProgDef {
        fun_defs: fun_defs,
        obj_defs: obj_defs,
        main_fun: main_fun,
      })
    },
    _ => Err(format!("program has to be a list")),
  }
}

pub fn fun_def_from_sexpr(elem: &sexpr::Elem) -> Result<spine::FunDef, String> {
  match *elem {
    sexpr::Elem::List(ref list) => 
      match list.get(0) {
        Some(&sexpr::Elem::Identifier(ref head)) if head.as_slice() == "fun" =>
          fun_def_from_sexprs(&list[1..]),
        _ => Err(format!("fun def must begin with 'fun'")),
      },
    _ => Err(format!("fun def must be a list")),
  }
}

fn fun_def_from_sexprs(list: &[sexpr::Elem]) -> Result<spine::FunDef, String> {
  if list.len() == 5 {
    let name = try!(fun_name_from_sexpr(&list[0]));
    let ret = try!(cont_name_from_sexpr(&list[1]));
    let captures = try!(vars_from_sexpr(&list[2]));
    let args = try!(vars_from_sexpr(&list[3]));
    let body = try!(term_from_sexpr(&list[4]));
    Ok(spine::FunDef { name: name, ret: ret, captures: captures, 
      args: args, body: body })
  } else {
    Err(format!("fun def must have 6 elems"))
  }
}

fn str_def_from_sexprs(list: &[sexpr::Elem]) -> Result<spine::ObjDef, String> {
  if list.len() == 2 {
    let name = try!(obj_name_from_sexpr(&list[0]));
    let bytes = match list[1] {
      sexpr::Elem::String(ref txt) => txt.clone().into_bytes(),
      sexpr::Elem::Int(byte) if byte >= 0 && byte < 256 => vec![byte as u8],
      _ => return Err(format!("string def must contain a string or bytes")),
    };
    Ok(spine::ObjDef { name: name, obj: spine::Obj::String(bytes) })
  } else if list.len() > 2 {
    let name = try!(obj_name_from_sexpr(&list[0]));
    let mut bytes = Vec::new();
    for elem in &list[1..] {
      match *elem {
        sexpr::Elem::Int(byte) if byte >= 0 && byte < 256 => bytes.push(byte as u8),
        _ => return Err(format!("string def must contain bytes")),
      }
    }
    Ok(spine::ObjDef { name: name, obj: spine::Obj::String(bytes) })
  } else {
    Err(format!("string def must have 3 elems"))
  }
}

pub fn term_from_sexpr(elem: &sexpr::Elem) -> Result<spine::Term, String> {
  match *elem {
    sexpr::Elem::List(ref list) => 
      match list.get(0) {
        Some(&sexpr::Elem::Identifier(ref head)) => match &head[..] {
          "letcont" => letcont_from_sexprs(&list[1..]),
          "letclos" => letclos_from_sexprs(&list[1..]),
          "call" => call_from_sexprs(&list[1..]),
          "extern-call" => extern_call_from_sexprs(&list[1..]),
          "cont" => cont_from_sexprs(&list[1..]),
          "branch" => branch_from_sexprs(&list[1..]),
          _ => Err(format!("unknown term head")),
        },
        _ => Err(format!("term list has to begin with an ident")),
      },
    _ => Err(format!("term has to be a list")),
  }
}

fn letcont_from_sexprs(elems: &[sexpr::Elem]) -> Result<spine::Term, String> {
  if elems.len() == 2 {
    let mut cont_defs = Vec::new();
    match elems[0] {
      sexpr::Elem::List(ref list) => 
        for elem in list.iter() {
          cont_defs.push(try!(cont_def_from_sexpr(elem)));
        },
      _ => return Err(format!("expected list of cont defs")),
    }

    Ok(spine::Term::Letcont(cont_defs, box try!(term_from_sexpr(&elems[1]))))
  } else {
    Err(format!("letcont must have cont defs and a term"))
  }
}

fn letclos_from_sexprs(elems: &[sexpr::Elem]) -> Result<spine::Term, String> {
  if elems.len() == 2 {
    let mut clos_defs = Vec::new();
    match elems[0] {
      sexpr::Elem::List(ref list) => 
        for elem in list.iter() {
          clos_defs.push(try!(clos_def_from_sexpr(elem)));
        },
      _ => return Err(format!("expected list of closure defs")),
    }

    Ok(spine::Term::Letclos(clos_defs, box try!(term_from_sexpr(&elems[1]))))
  } else {
    Err(format!("letclos must have closure defs and a term"))
  }
}

fn call_from_sexprs(elems: &[sexpr::Elem]) -> Result<spine::Term, String> {
  if elems.len() >= 2 {
    let fun = try!(val_from_sexpr(&elems[0]));
    let ret = try!(cont_name_from_sexpr(&elems[1]));
    let args = try!(vals_from_sexprs(&elems[2..]));
    Ok(spine::Term::Call(fun, ret, args))
  } else {
    Err(format!("call must have fun name, ret cont and args"))
  }
}

fn extern_call_from_sexprs(elems: &[sexpr::Elem]) -> Result<spine::Term, String> {
  if elems.len() >= 2 {
    let extern_name = try!(extern_name_from_sexpr(&elems[0]));
    let ret = try!(cont_name_from_sexpr(&elems[1]));
    let args = try!(vals_from_sexprs(&elems[2..]));
    Ok(spine::Term::ExternCall(extern_name, ret, args))
  } else {
    Err(format!("extern call must have fun name, ret cont and args"))
  }
}

fn cont_from_sexprs(elems: &[sexpr::Elem]) -> Result<spine::Term, String> {
  if elems.len() >= 1 {
    let cont_name = try!(cont_name_from_sexpr(&elems[0]));
    let args = try!(vals_from_sexprs(&elems[1..]));
    Ok(spine::Term::Cont(cont_name, args))
  } else {
    Err(format!("cont call must have cont name and args"))
  }
}

fn branch_from_sexprs(elems: &[sexpr::Elem]) -> Result<spine::Term, String> {
  if elems.len() == 3 {
    let boolval = try!(boolval_from_sexpr(&elems[0]));
    let then_cont = try!(cont_name_from_sexpr(&elems[1]));
    let else_cont = try!(cont_name_from_sexpr(&elems[2]));
    Ok(spine::Term::Branch(boolval, then_cont, else_cont))
  } else {
    Err(format!("branch must have boolval and then and else conts"))
  }
}

fn vals_from_sexprs(elems: &[sexpr::Elem]) -> Result<Vec<spine::Val>, String> {
  let mut vals = Vec::new();
  for elem in elems.iter() {
    vals.push(try!(val_from_sexpr(elem)));
  }
  Ok(vals)
}

fn val_from_sexpr(elem: &sexpr::Elem) -> Result<spine::Val, String> {
  match *elem {
    sexpr::Elem::Identifier(ref id) =>
      Ok(spine::Val::Var(spine::Var(id.clone()))),
    sexpr::Elem::Int(num) =>
      Ok(spine::Val::Int(num)),
    sexpr::Elem::List(ref elems) => match elems.get(0) {
      Some(&sexpr::Elem::Identifier(ref id)) => match &id[..] {
        "true" if elems.len() == 1 => Ok(spine::Val::True),
        "false" if elems.len() == 1 => Ok(spine::Val::False),
        "combinator" if elems.len() == 2 => 
          Ok(spine::Val::Combinator(try!(fun_name_from_sexpr(&elems[1])))),
        "obj" if elems.len() == 2 =>
          Ok(spine::Val::Obj(try!(obj_name_from_sexpr(&elems[1])))),
        _ => Err(format!("invalid val list")),
      },
      _ => Err(format!("invalid val list")),
    },
    _ => Err(format!("invalid val")),
  }
}

fn boolval_from_sexpr(elem: &sexpr::Elem) -> Result<spine::Boolval, String> {
  match *elem {
    sexpr::Elem::List(ref list) => {
      if list.len() == 2 {
        let val = try!(val_from_sexpr(&list[1]));
        match list[0] {
          sexpr::Elem::Identifier(ref head) => match &head[..] {
            "is-true" => Ok(spine::Boolval::IsTrue(val)),
            "is-false" => Ok(spine::Boolval::IsFalse(val)),
            _ => Err(format!("unknown boolval head")),
          },
          _ => Err(format!("boolval list must begin with id")),
        }
      } else {
        Err(format!("boolval must have a head and a val"))
      }
    },
    _ => Err(format!("boolval must be a list")),
  }
}

fn cont_def_from_sexpr(elem: &sexpr::Elem) -> Result<spine::ContDef, String> {
  match *elem {
    sexpr::Elem::List(ref list) => {
      if list.len() == 3 {
        let name = try!(cont_name_from_sexpr(&list[0]));
        let args = try!(vars_from_sexpr(&list[1]));
        let body = try!(term_from_sexpr(&list[2]));
        Ok(spine::ContDef { name: name, args: args, body: body })
      } else {
        Err(format!("cont def must have name, args and body"))
      }
    }, _ => Err(format!("cont def must be a list"))
  }
}

fn clos_def_from_sexpr(elem: &sexpr::Elem) -> Result<spine::ClosureDef, String> {
  match *elem {
    sexpr::Elem::List(ref list) => {
      if list.len() >= 2 {
        let var = try!(var_from_sexpr(&list[0]));
        let fun_name = try!(fun_name_from_sexpr(&list[1]));
        let captures = try!(vals_from_sexprs(&list[2..]));
        Ok(spine::ClosureDef { var: var, fun_name: fun_name, captures: captures })
      } else {
        Err(format!("cont def must have name, args and body"))
      }
    }, _ => Err(format!("cont def must be a list"))
  }
}


fn vars_from_sexpr(elem: &sexpr::Elem) -> Result<Vec<spine::Var>, String> {
  match *elem {
    sexpr::Elem::List(ref list) => {
      let mut vars = Vec::new();
      for elem in list.iter() {
        vars.push(try!(var_from_sexpr(elem)));
      }
      Ok(vars)
    },
    _ => Err(format!("expected list of vars")),
  }
}

fn var_from_sexpr(elem: &sexpr::Elem) -> Result<spine::Var, String> {
  match *elem {
    sexpr::Elem::Identifier(ref id) =>
      Ok(spine::Var(id.clone())),
    _ => Err(format!("expected a variable")),
  }
}

fn fun_name_from_sexpr(elem: &sexpr::Elem) -> Result<spine::FunName, String> {
  match *elem {
    sexpr::Elem::Identifier(ref id) =>
      Ok(spine::FunName(id.clone())),
    _ => Err(format!("expected a fun name")),
  }
}

fn cont_name_from_sexpr(elem: &sexpr::Elem) -> Result<spine::ContName, String> {
  match *elem {
    sexpr::Elem::Identifier(ref id) =>
      Ok(spine::ContName(id.clone())),
    _ => Err(format!("expected a cont name")),
  }
}

fn extern_name_from_sexpr(elem: &sexpr::Elem) -> Result<spine::ExternName, String> {
  match *elem {
    sexpr::Elem::Identifier(ref id) =>
      Ok(spine::ExternName(id.clone())),
    _ => Err(format!("expected an extern name")),
  }
}

fn obj_name_from_sexpr(elem: &sexpr::Elem) -> Result<spine::ObjName, String> {
  match *elem {
    sexpr::Elem::Identifier(ref id) =>
      Ok(spine::ObjName(id.clone())),
    _ => Err(format!("expected an object name")),
  }
}


#[cfg(test)]
mod test {
  use sexpr;
  use spine::helpers::*;

  fn parse_prog(txt: &str) -> ProgDef {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    sexpr::to_spine::prog_from_sexpr(&sexpr).unwrap()
  }

  fn parse_fun_def(txt: &str) -> FunDef {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    sexpr::to_spine::fun_def_from_sexpr(&sexpr).unwrap()
  }

  fn parse_term(txt: &str) -> Term {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    sexpr::to_spine::term_from_sexpr(&sexpr).unwrap()
  }

  fn byte_str(txt: &str) -> Vec<u8> {
    txt.to_string().into_bytes()
  }

  #[test]
  fn test_empty_prog() {
    assert_eq!(parse_prog("(program start)"),
      ProgDef {
        main_fun: fun("start"),
        fun_defs: vec![],
        obj_defs: vec![],
      });
  }

  #[test]
  fn test_prog_with_fun() {
    assert_eq!(parse_prog("(program start (fun start r () () (cont r)))"),
      ProgDef {
        main_fun: fun("start"),
        fun_defs: vec![
          FunDef {
            name: fun("start"),
            ret: cont("r"),
            captures: vec![],
            args: vec![],
            body: Cont(cont("r"), vec![]),
          }
        ],
        obj_defs: vec![],
      });
  }

  #[test]
  fn test_prog_with_string() {
    assert_eq!(parse_prog("(program main (string s1 \"spiral\"))"),
      ProgDef {
        main_fun: fun("main"),
        fun_defs: vec![],
        obj_defs: vec![
          ObjDef { name: obj("s1"), obj: Obj::String(byte_str("spiral")) },
        ],
      });
  }

  #[test]
  fn test_fun_def() {
    assert_eq!(parse_fun_def("(fun compute-zero ret (x y) (a b) (cont ret 0))"),
      FunDef {
        name: fun("compute-zero"),
        ret: cont("ret"),
        captures: vec![var("x"), var("y")],
        args: vec![var("a"), var("b")],
        body: Cont(cont("ret"), vec![Int(0)]),
      });
  }

  #[test]
  fn test_letcont() {
    assert_eq!(parse_term(
        "(letcont (
          (c1 (a1 b1) (cont r a1))
          (c2 (a2 b2 c2) (cont r b2))
        ) (cont c2 1 2 3))"),
      Letcont(vec![
        ContDef { name: cont("c1"), args: vec![var("a1"), var("b1")],
          body: Cont(cont("r"), vec![var_val("a1")]) },
        ContDef { name: cont("c2"), args: vec![var("a2"), var("b2"), var("c2")],
          body: Cont(cont("r"), vec![var_val("b2")]) }, 
        ], box Cont(cont("c2"), vec![Int(1), Int(2), Int(3)])));
  }

  #[test]
  fn test_letclos() {
    assert_eq!(parse_term(
        "(letclos (
          (clos1 fun1 a b c)
          (clos2 fun2 10 y)
        ) (cont cc))"),
      Letclos(vec![
        ClosureDef { var: var("clos1"), fun_name: fun("fun1"),
          captures: vec![var_val("a"), var_val("b"), var_val("c")] },
        ClosureDef { var: var("clos2"), fun_name: fun("fun2"),
          captures: vec![Int(10), var_val("y")] },
        ], box Cont(cont("cc"), vec![])));
  }

  #[test]
  fn test_call() {
    assert_eq!(parse_term("(call f k a b 2)"),
      Call(var_val("f"), cont("k"), vec![var_val("a"), var_val("b"), Int(2)]));
  }

  #[test]
  fn test_extern_call() {
    assert_eq!(parse_term("(extern-call f k a b 2)"),
      ExternCall(ext_name("f"), cont("k"),
        vec![var_val("a"), var_val("b"), Int(2)]));
  }

  #[test]
  fn test_cont() {
    assert_eq!(parse_term("(cont k 0 1 b)"),
      Cont(cont("k"), vec![Int(0), Int(1), var_val("b")]));
  }

  #[test]
  fn test_branch() {
    assert_eq!(parse_term("(branch (is-true 0) ok not-ok)"),
      Branch(IsTrue(Int(0)), cont("ok"), cont("not-ok")));
    assert_eq!(parse_term("(branch (is-false f) ok not-ok)"),
      Branch(IsFalse(var_val("f")), cont("ok"), cont("not-ok")));
  }

  #[test]
  fn test_combinator_val() {
    assert_eq!(parse_term("(cont cc (combinator Y))"),
      Cont(cont("cc"), vec![combinator_val("Y")]));
  }

  #[test]
  fn test_true_false_val() {
    assert_eq!(parse_term("(cont cc (true) (false))"),
      Cont(cont("cc"), vec![True, False]));
  }

  #[test]
  fn test_var_val() {
    assert_eq!(parse_term("(cont cc x y z)"),
      Cont(cont("cc"), vec![var_val("x"), var_val("y"), var_val("z")]));
    assert_eq!(parse_term("(cont cc true false)"),
      Cont(cont("cc"), vec![var_val("true"), var_val("false")]));
  }

  #[test]
  fn test_obj_val() {
    assert_eq!(parse_term("(cont cc (obj o1) (obj o2))"),
      Cont(cont("cc"), vec![Val::Obj(obj("o1")), Val::Obj(obj("o2"))]));
  }
}
