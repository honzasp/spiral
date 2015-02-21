use sexpr;
use spiral::*;

pub fn prog_from_sexpr(prog: &sexpr::Elem) -> Result<Prog, String> {
  match *prog {
    sexpr::Elem::List(ref list) => match list.get(0) {
      Some(&sexpr::Elem::Identifier(ref head)) if head.as_slice() == "program" =>
        Ok(Prog { body: try!(stmts_from_sexprs(&list[1..])) }),
      _ => Err(format!("program has to begin with 'program'")),
    },
    _ => Err(format!("program has to be list")),
  }
}

fn stmts_from_sexprs(elems: &[sexpr::Elem]) -> Result<Vec<Stmt>, String> {
  let mut stmts = Vec::new();
  for elem in elems.iter() {
    stmts.push(try!(stmt_from_sexpr(elem)));
  }
  Ok(stmts)
}

pub fn stmt_from_sexpr(stmt: &sexpr::Elem) -> Result<Stmt, String> {
  match *stmt {
    sexpr::Elem::List(ref list) => match list.first() {
      Some(&sexpr::Elem::Identifier(ref id)) => match id.as_slice() {
        "fun" => return fun_def_from_sexprs(list.tail()),
        "var" => return var_def_from_sexprs(list.tail()),
        _ => { },
      },
      _ => { },
    },
    _ => { },
  }
  return Ok(Stmt::Expr(try!(expr_from_sexpr(stmt))));
}

fn fun_def_from_sexprs(elems: &[sexpr::Elem]) -> Result<Stmt, String> {
  if elems.len() >= 2 {
    let name = try!(fun_name_from_sexpr(&elems[0]));
    let args = try!(vars_from_sexpr(&elems[1]));
    let stmts = try!(stmts_from_sexprs(&elems[2..]));
    Ok(Stmt::Fun(name, args, stmts))
  } else {
    return Err(format!("fun def expects at least fun name and args"));
  }
}

fn var_def_from_sexprs(elems: &[sexpr::Elem]) -> Result<Stmt, String> {
  if elems.len() == 2 {
    let var = try!(var_from_sexpr(&elems[0]));
    let value = try!(expr_from_sexpr(&elems[1]));
    Ok(Stmt::Var(var, value))
  } else {
    return Err(format!("var def expects just var name and expr"));
  }
}

pub fn expr_from_sexpr(expr: &sexpr::Elem) -> Result<Expr, String> {
  match *expr {
    sexpr::Elem::Identifier(ref name) =>
      Ok(Expr::Var(Var(name.clone()))),
    sexpr::Elem::Number(number) =>
      Ok(Expr::Literal(number)),
    sexpr::Elem::List(ref list) => match list.get(0) {
      Some(&sexpr::Elem::Identifier(ref id)) => match id.as_slice() {
        "if" => if_from_sexpr(&list[1..]),
        "cond" => cond_from_sexpr(&list[1..]),
        "when" => when_from_sexpr(&list[1..]),
        "unless" => unless_from_sexpr(&list[1..]),
        "do" => do_from_sexpr(&list[1..]),
        "and" => and_from_sexpr(&list[1..]),
        "or" => or_from_sexpr(&list[1..]),
        "begin" => begin_from_sexpr(&list[1..]),
        "let" => let_from_sexpr(&list[1..]),
        _ => call_from_sexpr(id, &list[1..]),
      },
      _ => Err(format!("list expr has to begin with an identifier")),
    }
  }
}

fn exprs_from_sexprs(elems: &[sexpr::Elem]) -> Result<Vec<Expr>, String> {
  let mut exprs = Vec::new();
  for elem in elems.iter() {
    exprs.push(try!(expr_from_sexpr(elem)));
  }
  Ok(exprs)
}

fn if_from_sexpr(elems: &[sexpr::Elem]) -> Result<Expr, String> {
  if elems.len() == 3 {
    let cond = try!(expr_from_sexpr(&elems[0]));
    let then_e = try!(expr_from_sexpr(&elems[1]));
    let else_e = try!(expr_from_sexpr(&elems[2]));
    Ok(Expr::If(box cond, box then_e, box else_e))
  } else {
    return Err(format!("'if' expects cond, then and else exprs"));
  }
}

fn cond_from_sexpr(_elems: &[sexpr::Elem]) -> Result<Expr, String> {
  Err(format!("'cond' is not implemented yet"))
}

fn when_from_sexpr(_elems: &[sexpr::Elem]) -> Result<Expr, String> {
  Err(format!("'when' is not implemented yet"))
}

fn unless_from_sexpr(_elems: &[sexpr::Elem]) -> Result<Expr, String> {
  Err(format!("'unless' is not implemented yet"))
}

fn do_from_sexpr(elems: &[sexpr::Elem]) -> Result<Expr, String> {
  fn var_binds_from_sexpr(elem: &sexpr::Elem) 
    -> Result<Vec<(Var, Expr, Expr)>, String>
  {
    match *elem {
      sexpr::Elem::List(ref elems) => {
        let mut var_binds = Vec::new();
        for bind in elems.iter() {
          var_binds.push(try!(var_bind_from_sexpr(bind)));
        }
        Ok(var_binds)
      },
      _ => Err(format!("var binds in 'do' must be a list")),
    }
  }

  fn var_bind_from_sexpr(elem: &sexpr::Elem) 
    -> Result<(Var, Expr, Expr), String> 
  {
    match *elem {
      sexpr::Elem::List(ref bind) => {
        if bind.len() == 3 {
          let var = try!(var_from_sexpr(&bind[0]));
          let init = try!(expr_from_sexpr(&bind[1]));
          let step = try!(expr_from_sexpr(&bind[2]));
          Ok((var, init, step))
        } else {
          Err(format!("var bind list in 'do' must have var, init and next"))
        }
      },
      _ => Err(format!("var bind in 'do' must be a list")),
    }
  }

  fn exit_from_sexpr(elem: &sexpr::Elem) -> Result<(Expr, Vec<Stmt>), String> {
    match *elem {
      sexpr::Elem::List(ref list) => 
        if list.len() >= 1 {
          let exit_cond = try!(expr_from_sexpr(&list[0]));
          let exit_stmts = try!(stmts_from_sexprs(&list[1..]));
          Ok((exit_cond, exit_stmts))
        } else {
          Err(format!("'do' must have an exit condition"))
        },
      _ => Err(format!("'do' exit must be a list")),
    }
  }

  if elems.len() >= 2 {
    let var_binds = try!(var_binds_from_sexpr(&elems[0]));
    let (exit_cond, exit_stmts) = try!(exit_from_sexpr(&elems[1]));
    let body_stmts = try!(stmts_from_sexprs(&elems[2..]));
    Ok(Expr::Do(var_binds, box exit_cond, exit_stmts, body_stmts))
  } else {
    return Err(format!("'do' expects at least var binds and exit cond"));
  }
}

fn and_from_sexpr(elems: &[sexpr::Elem]) -> Result<Expr, String> {
  Ok(Expr::And(try!(exprs_from_sexprs(elems))))
}

fn or_from_sexpr(elems: &[sexpr::Elem]) -> Result<Expr, String> {
  Ok(Expr::Or(try!(exprs_from_sexprs(elems))))
}

fn begin_from_sexpr(elems: &[sexpr::Elem]) -> Result<Expr, String> {
  Ok(Expr::Begin(try!(stmts_from_sexprs(elems))))
}

fn let_from_sexpr(elems: &[sexpr::Elem]) -> Result<Expr, String> {
  fn var_binds_from_sexpr(elem: &sexpr::Elem) -> Result<Vec<(Var, Expr)>, String> {
    match *elem {
      sexpr::Elem::List(ref elems) => {
        let mut var_binds = Vec::new();
        for bind in elems.iter() {
          var_binds.push(try!(var_bind_from_sexpr(bind)));
        }
        Ok(var_binds)
      },
      _ => Err(format!("var binds in 'let' must be a list")),
    }
  }

  fn var_bind_from_sexpr(elem: &sexpr::Elem) -> Result<(Var, Expr), String> {
    match *elem {
      sexpr::Elem::List(ref bind) => {
        if bind.len() == 2 {
          let var = try!(var_from_sexpr(&bind[0]));
          let value = try!(expr_from_sexpr(&bind[1]));
          Ok((var, value))
        } else {
          Err(format!("var bind in 'let' can have 2 or 3 elems"))
        }
      },
      _ => Err(format!("var bind in 'let' must be a list")),
    }
  }

  if elems.len() >= 1 {
    let binds = try!(var_binds_from_sexpr(&elems[0]));
    let body = try!(stmts_from_sexprs(&elems[1..]));
    Ok(Expr::Let(binds, body))
  } else {
    Err(format!("'let' cannot be empty"))
  }
}

fn call_from_sexpr(head: &String, elems: &[sexpr::Elem]) -> Result<Expr, String> {
  let args = try!(exprs_from_sexprs(elems));
  Ok(Expr::Call(FunName(head.clone()), args))
}

fn fun_name_from_sexpr(elem: &sexpr::Elem) -> Result<FunName, String> {
  match *elem {
    sexpr::Elem::Identifier(ref name) => Ok(FunName(name.clone())),
    _ => Err(format!("fun name must be a string")),
  }
}

fn var_from_sexpr(elem: &sexpr::Elem) -> Result<Var, String> {
  match *elem {
    sexpr::Elem::Identifier(ref name) => Ok(Var(name.clone())),
    _ => Err(format!("var must be a string")),
  }
}

fn vars_from_sexpr(elem: &sexpr::Elem) -> Result<Vec<Var>, String> {
  match *elem {
    sexpr::Elem::List(ref elems) => {
      let mut vars = Vec::new();
      for var in elems.iter() {
        vars.push(try!(var_from_sexpr(var)));
      }
      Ok(vars)
    },
    _ => Err(format!("expected list of vars")),
  }
}

#[cfg(test)]
mod test {
  use sexpr;
  use spiral as s;

  fn parse_prog(txt: &str) -> s::Prog {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    sexpr::to_spiral::prog_from_sexpr(&sexpr).unwrap()
  }

  fn parse_stmt(txt: &str) -> s::Stmt {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    sexpr::to_spiral::stmt_from_sexpr(&sexpr).unwrap()
  }

  fn parse_expr(txt: &str) -> s::Expr {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    sexpr::to_spiral::expr_from_sexpr(&sexpr).unwrap()
  }

  fn fun_name(id: &str) -> s::FunName {
    s::FunName(id.to_string())
  }

  fn var(id: &str) -> s::Var {
    s::Var(id.to_string())
  }

  fn lit_expr(num: f32) -> s::Expr {
    s::Expr::Literal(num)
  }

  fn var_expr(id: &str) -> s::Expr {
    s::Expr::Var(var(id))
  }

  #[test]
  fn test_prog() {
    assert_eq!(parse_prog("(program (var x 10) (fun get-x () x))"),
      s::Prog { body: vec![
        s::Stmt::Var(var("x"), lit_expr(10.0)),
        s::Stmt::Fun(fun_name("get-x"), vec![], vec![
            s::Stmt::Expr(var_expr("x")),
          ]),
      ]});
  }

  #[test]
  fn test_fun_def() {
    assert_eq!(parse_stmt("(fun f (a b) 3)"),
      s::Stmt::Fun(fun_name("f"), vec![var("a"), var("b")], vec![
          s::Stmt::Expr(lit_expr(3.0))
        ]));
  }

  #[test]
  fn test_var_def() {
    assert_eq!(parse_stmt("(var x 3)"),
      s::Stmt::Var(var("x"), lit_expr(3.0)));
  }

  #[test]
  fn test_if_expr() {
    assert_eq!(parse_expr("(if a 2 3)"),
      s::Expr::If(box var_expr("a"), box lit_expr(2.0), box lit_expr(3.0)));
  }

  #[test] #[ignore]
  fn test_cond_expr() {
    assert_eq!(parse_expr("(cond (a 1 2) (b 2) (else 3))"),
      s::Expr::Cond(vec![
          (var_expr("a"), vec![
              s::Stmt::Expr(lit_expr(1.0)),
              s::Stmt::Expr(lit_expr(2.0)),
            ]),
          (var_expr("b"), vec![
              s::Stmt::Expr(lit_expr(1.0)),
            ]),
        ],
        Some(vec![
            s::Stmt::Expr(lit_expr(3.0)),
          ]))
      );

    assert_eq!(parse_expr("(cond (a 1 2) (b 2))"),
      s::Expr::Cond(vec![
          (var_expr("a"), vec![
              s::Stmt::Expr(lit_expr(1.0)),
              s::Stmt::Expr(lit_expr(2.0)),
            ]),
          (var_expr("b"), vec![
              s::Stmt::Expr(lit_expr(1.0)),
            ]),
        ],
        None)
      );
  }

  #[test] #[ignore]
  fn test_when_expr() {
    assert_eq!(parse_expr("(when a 1 2)"),
      s::Expr::When(box var_expr("a"), vec![
          s::Stmt::Expr(lit_expr(1.0)),
          s::Stmt::Expr(lit_expr(2.0)),
        ]));
  }

  #[test] #[ignore]
  fn test_unless_expr() {
    assert_eq!(parse_expr("(unless b 1 2)"),
      s::Expr::Unless(box var_expr("b"), vec![
          s::Stmt::Expr(lit_expr(1.0)),
          s::Stmt::Expr(lit_expr(2.0)),
        ]));
  }

  #[test]
  fn test_do_expr() {
    assert_eq!(parse_expr("(do ((i 1 j) (j 0 i)) (x 0) 1 2)"),
      s::Expr::Do(
        vec![
          (var("i"), lit_expr(1.0), var_expr("j")),
          (var("j"), lit_expr(0.0), var_expr("i")),
        ],
        box var_expr("x"),
        vec![
          s::Stmt::Expr(lit_expr(0.0)),
        ],
        vec![
          s::Stmt::Expr(lit_expr(1.0)),
          s::Stmt::Expr(lit_expr(2.0)),
        ]
      ));
  }

  #[test]
  fn test_and_expr() {
    assert_eq!(parse_expr("(and 1 2 3)"),
      s::Expr::And(vec![lit_expr(1.0), lit_expr(2.0), lit_expr(3.0)]));
  }

  #[test]
  fn test_or_expr() {
    assert_eq!(parse_expr("(or 1 b 3)"),
      s::Expr::Or(vec![lit_expr(1.0), var_expr("b"), lit_expr(3.0)]));
  }

  #[test]
  fn test_begin_expr() {
    assert_eq!(parse_expr("(begin 1 2)"),
      s::Expr::Begin(vec![
        s::Stmt::Expr(lit_expr(1.0)),
        s::Stmt::Expr(lit_expr(2.0)),
      ]));
  }

  #[test]
  fn test_let_expr() {
    assert_eq!(parse_expr("(let ((a 1) (b 2)) a b)"),
      s::Expr::Let(vec![
          (var("a"), lit_expr(1.0)),
          (var("b"), lit_expr(2.0)),
        ], vec![
          s::Stmt::Expr(var_expr("a")),
          s::Stmt::Expr(var_expr("b")),
        ]));
  }

  #[test]
  fn test_call_expr() {
    assert_eq!(parse_expr("(+ 1 2)"),
      s::Expr::Call(fun_name("+"), vec![lit_expr(1.0), lit_expr(2.0)]));
  }
}
