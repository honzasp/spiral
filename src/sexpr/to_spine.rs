use sexpr;
use spine;

pub fn prog_from_sexpr(prog: &sexpr::Elem) -> Result<spine::ProgDef, String> {
  match *prog {
    sexpr::Elem::List(ref list) => 
      if list.len() == 3 {
        match list[0] {
          sexpr::Elem::Identifier(ref head) if head.as_slice() == "program" => (),
          _ => return Err(format!("program has to begin with 'program'")),
        };

        let halt_cont = try!(cont_name_from_sexpr(&list[1]));
        let body = try!(term_from_sexpr(&list[2]));
        Ok(spine::ProgDef {
          body: body,
          halt_cont: halt_cont,
        })
      } else {
        Err(format!("program must have 3 elems"))
      },
    _ => Err(format!("program must be a list")),
  }
}

pub fn term_from_sexpr(elem: &sexpr::Elem) -> Result<spine::Term, String> {
  match *elem {
    sexpr::Elem::List(ref list) => 
      match list.get(0) {
        Some(&sexpr::Elem::Identifier(ref head)) => match &head[..] {
          "letcont" => letcont_from_sexprs(&list[1..]),
          "letfun" => letfun_from_sexprs(&list[1..]),
          "letobj" => letobj_from_sexprs(&list[1..]),
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
  if elems.len() >= 1 {
    let mut cont_defs = Vec::new();
    for elem in elems.init().iter() {
      cont_defs.push(try!(cont_def_from_sexpr(elem)));
    }
    let body = try!(term_from_sexpr(&elems[elems.len() - 1]));
    Ok(spine::Term::Letcont(cont_defs, box body))
  } else {
    Err(format!("letcont must have cont defs and a term"))
  }
}

fn letfun_from_sexprs(elems: &[sexpr::Elem]) -> Result<spine::Term, String> {
  if elems.len() >= 1 {
    let mut fun_defs = Vec::new();
    for elem in elems.init().iter() {
      fun_defs.push(try!(fun_def_from_sexpr(elem)));
    }
    let body = try!(term_from_sexpr(&elems[elems.len() - 1]));
    Ok(spine::Term::Letfun(fun_defs, box body))
  } else {
    Err(format!("letfun must have fun defs and a term"))
  }
}

fn letobj_from_sexprs(elems: &[sexpr::Elem]) -> Result<spine::Term, String> {
  if elems.len() == 2 {
    let obj_def = try!(obj_def_from_sexpr(&elems[0]));
    let body = try!(term_from_sexpr(&elems[1]));
    Ok(spine::Term::Letobj(obj_def, box body))
  } else {
    Err(format!("letobj must have obj def and a term"))
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
        _ => Err(format!("invalid val list head")),
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

fn fun_def_from_sexpr(elem: &sexpr::Elem) -> Result<spine::FunDef, String> {
  match *elem {
    sexpr::Elem::List(ref list) => 
      if list.len() == 5 {
        let var = try!(var_from_sexpr(&list[0]));
        let ret = try!(cont_name_from_sexpr(&list[1]));
        let captures = try!(vars_from_sexpr(&list[2]));
        let args = try!(vars_from_sexpr(&list[3]));
        let body = try!(term_from_sexpr(&list[4]));
        Ok(spine::FunDef { var: var, ret: ret, captures: captures, 
          args: args, body: body })
      } else {
        Err(format!("fun def must have 5 elems"))
      },
    _ => Err(format!("fun def must be a list")),
  }
}

fn obj_def_from_sexpr(elem: &sexpr::Elem) -> Result<spine::ObjDef, String> {
  match *elem {
    sexpr::Elem::List(ref list) => match list.get(0) {
      Some(&sexpr::Elem::Identifier(ref head)) => match &head[..] {
        "string" => str_def_from_sexprs(&list[1..]),
        "double" => dbl_def_from_sexprs(&list[1..]),
        _ => Err(format!("unknown obj def")),
      },
      _ => Err(format!("obj def must begin with an ident")),
    },
    _ => Err(format!("obj def must be a list")),
  }
}


fn str_def_from_sexprs(list: &[sexpr::Elem]) -> Result<spine::ObjDef, String> {
  if list.len() == 2 {
    let var = try!(var_from_sexpr(&list[0]));
    let bytes = match list[1] {
      sexpr::Elem::String(ref txt) => txt.clone().into_bytes(),
      sexpr::Elem::Int(byte) if byte >= 0 && byte < 256 => vec![byte as u8],
      _ => return Err(format!("string def must contain a string or bytes")),
    };
    Ok(spine::ObjDef { var: var, obj: spine::Obj::String(bytes) })
  } else if list.len() > 2 {
    let var = try!(var_from_sexpr(&list[0]));
    let mut bytes = Vec::new();
    for elem in &list[1..] {
      match *elem {
        sexpr::Elem::Int(byte) if byte >= 0 && byte < 256 => bytes.push(byte as u8),
        _ => return Err(format!("string def must contain bytes")),
      }
    }
    Ok(spine::ObjDef { var: var, obj: spine::Obj::String(bytes) })
  } else {
    Err(format!("string def must have 3 elems"))
  }
}

fn dbl_def_from_sexprs(list: &[sexpr::Elem]) -> Result<spine::ObjDef, String> {
  if list.len() == 2 {
    let var = try!(var_from_sexpr(&list[0]));
    let number = match list[1] {
      sexpr::Elem::Double(num) => num,
      sexpr::Elem::Int(num) => num as f64,
      _ => return Err(format!("double value must be a number")),
    };
    Ok(spine::ObjDef { var: var, obj: spine::Obj::Double(number) })
  } else {
    Err(format!("double def must have 3 elems"))
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

#[cfg(test)]
mod test {
  use sexpr;
  use spine;
  use spine::{ProgDef, FunDef, ContDef, ObjDef, Obj, Term};
  use spine::Term::*;
  use spine::Val::{Int, True, False};
  use spine::Boolval::{IsTrue, IsFalse};

  fn parse_prog(txt: &str) -> ProgDef {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    sexpr::to_spine::prog_from_sexpr(&sexpr).unwrap()
  }

  fn parse_term(txt: &str) -> Term {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    sexpr::to_spine::term_from_sexpr(&sexpr).unwrap()
  }

  fn byte_str(txt: &str) -> Vec<u8> {
    txt.to_string().into_bytes()
  }

  fn var(i: &str) -> spine::Var { spine::Var(i.to_string()) }
  fn ext_name(i: &str) -> spine::ExternName { spine::ExternName(i.to_string()) }
  fn cont(i: &str) -> spine::ContName { spine::ContName(i.to_string()) }
  fn var_val(i: &str) -> spine::Val { spine::Val::Var(var(i)) }

  #[test]
  fn test_empty_prog() {
    assert_eq!(parse_prog("(program halt (cont halt))"),
      ProgDef {
        body: Cont(cont("halt"), vec![]),
        halt_cont: cont("halt"),
      });
  }

  #[test]
  fn test_letcont() {
    assert_eq!(parse_term(
        "(letcont
          (c1 (a1 b1) (cont r a1))
          (c2 (a2 b2 c2) (cont r b2))
          (cont c2 1 2 3))"),
      Letcont(vec![
        ContDef { name: cont("c1"), args: vec![var("a1"), var("b1")],
          body: Cont(cont("r"), vec![var_val("a1")]) },
        ContDef { name: cont("c2"), args: vec![var("a2"), var("b2"), var("c2")],
          body: Cont(cont("r"), vec![var_val("b2")]) }, 
        ], box Cont(cont("c2"), vec![Int(1), Int(2), Int(3)])));
  }

  #[test]
  fn test_letfun() {
    assert_eq!(parse_term(
        "(letfun 
          (fun1 r (x) (a) (cont r 0))
          (fun2 q (x y) (a b) (cont r 0))
          (cont cc))"),
      Letfun(vec![
          FunDef { var: var("fun1"), ret: cont("r"),
            captures: vec![var("x")], args: vec![var("a")],
            body: Cont(cont("r"), vec![Int(0)]) },
          FunDef { var: var("fun2"), ret: cont("q"),
            captures: vec![var("x"), var("y")], args: vec![var("a"), var("b")],
            body: Cont(cont("r"), vec![Int(0)]) },
        ], box Cont(cont("cc"), vec![])));
  }

  #[test]
  fn test_letobj_string() {
    assert_eq!(parse_term("(letobj (string s1 \"spiral\") (cont cc))"),
      Letobj(ObjDef {
          var: var("s1"),
          obj: Obj::String(byte_str("spiral")) 
        }, box Cont(cont("cc"), vec![])));
  }

  #[test]
  fn test_letobj_double() {
    assert_eq!(parse_term("(letobj (double half 0.5) (cont cc))"),
      Letobj(ObjDef {
          var: var("half"),
          obj: Obj::Double(0.5) 
        }, box Cont(cont("cc"), vec![])));
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
}
