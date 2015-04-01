use sexpr;
use spiral::*;

pub fn prog_from_sexpr(prog: &sexpr::Elem) -> Result<Prog, String> {
  match *prog {
    sexpr::Elem::List(ref list) => match list.get(0) {
      Some(&sexpr::Elem::Identifier(ref head)) if head.as_slice() == "program" =>
        Ok(Prog { stmts: try!(stmts_from_sexprs(&list[1..])) }),
      _ => Err(format!("program has to begin with 'program'")),
    },
    _ => Err(format!("program has to be a list")),
  }
}

pub fn mod_from_sexpr(module: &sexpr::Elem) -> Result<Mod, String> {
  match *module {
    sexpr::Elem::List(ref list) => {
      if list.len() >= 2 {
        let _ = try!(match list[0] {
          sexpr::Elem::Identifier(ref head) if head.as_slice() == "module" => Ok(()),
          _ => Err(format!("module has to begin with 'module'")),
        });

        let mod_name = try!(mod_name_from_sexpr(&list[1]));
        let decls = try!(decls_from_sexprs(&list[2..]));
        Ok(Mod { name: mod_name, decls: decls })
      } else {
        Err(format!("module must have at least 2 elems"))
      }
    },
    _ => Err(format!("module has to be a list")),
  }
}

fn decls_from_sexprs(elems: &[sexpr::Elem]) -> Result<Vec<Decl>, String> {
  let mut decls = Vec::new();
  for elem in elems.iter() {
    decls.push(try!(decl_from_sexpr(elem)));
  }
  Ok(decls)
}

pub fn decl_from_sexpr(decl: &sexpr::Elem) -> Result<Decl, String> {
  match *decl {
    sexpr::Elem::List(ref list) => match list.first() {
      Some(&sexpr::Elem::Identifier(ref id)) => match id.as_slice() {
        "export" => return export_from_sexprs(list.tail()),
        _ => (),
      },
      _ => (),
    },
    _ => (),
  }
  return Ok(Decl::Stmt(try!(stmt_from_sexpr(decl))));
}

fn export_from_sexprs(elems: &[sexpr::Elem]) -> Result<Decl, String> {
  let mut vars = Vec::new();
  for elem in elems.iter() {
    vars.push(try!(var_from_sexpr(elem)));
  }
  Ok(Decl::Export(vars))
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
        "import" => return import_from_sexprs(list.tail()),
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
    let fun_def = FunDef {
      var: try!(var_from_sexpr(&elems[0])),
      args: try!(vars_from_sexpr(&elems[1])),
      stmts: try!(stmts_from_sexprs(&elems[2..])),
    };
    Ok(Stmt::Fun(fun_def))
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

fn import_from_sexprs(elems: &[sexpr::Elem]) -> Result<Stmt, String> {
  let mut import_defs = Vec::new();
  for elem in elems.iter() {
    import_defs.push(try!(import_def_from_sexpr(elem)));
  }
  Ok(Stmt::Import(import_defs))
}

fn import_def_from_sexpr(elem: &sexpr::Elem) -> Result<ImportDef, String> {
  fn only_from_sexprs(elems: &[sexpr::Elem]) -> Result<ImportDef, String> {
    if elems.len() >= 1 {
      let def = try!(import_def_from_sexpr(&elems[0]));
      let vars = try!(vars_from_sexprs(&elems[1..]));
      Ok(ImportDef::Only(box def, vars))
    } else {
      Err(format!("'only' import def cannot be empty"))
    }
  }

  fn except_from_sexprs(elems: &[sexpr::Elem]) -> Result<ImportDef, String> {
    if elems.len() >= 1 {
      let def = try!(import_def_from_sexpr(&elems[0]));
      let vars = try!(vars_from_sexprs(&elems[1..]));
      Ok(ImportDef::Except(box def, vars))
    } else {
      Err(format!("'except' import def cannot be empty"))
    }
  }

  fn prefix_from_sexprs(elems: &[sexpr::Elem]) -> Result<ImportDef, String> {
    if elems.len() == 2 {
      let def = try!(import_def_from_sexpr(&elems[0]));
      let prefix = try!(var_from_sexpr(&elems[1]));
      Ok(ImportDef::Prefix(box def, prefix))
    } else {
      Err(format!("'prefix' import must have an import def and a prefix"))
    }
  }

  match *elem {
    sexpr::Elem::Identifier(ref name) =>
      Ok(ImportDef::Mod(ModName(name.clone()))),
    sexpr::Elem::List(ref elems) => 
      match elems.get(0) {
        Some(&sexpr::Elem::Identifier(ref head)) => match &head[..] {
          "only" => only_from_sexprs(&elems[1..]),
          "except" => except_from_sexprs(&elems[1..]),
          "prefix" => prefix_from_sexprs(&elems[1..]),
          _ => Err(format!("unknown import def head name")),
        },
        _ => Err(format!("unknown import def head")),
      },
    _ => Err(format!("invalid import def")),
  }
}

pub fn expr_from_sexpr(expr: &sexpr::Elem) -> Result<Expr, String> {
  match *expr {
    sexpr::Elem::Identifier(ref name) =>
      Ok(Expr::Var(Var(name.clone()))),
    sexpr::Elem::Int(number) =>
      Ok(Expr::Int(number)),
    sexpr::Elem::Float(_) =>
      Err(format!("floats are not supported yet")),
    sexpr::Elem::String(ref txt) =>
      Ok(Expr::String(txt.clone())),
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
        "lambda" => lambda_from_sexpr(&list[1..]),
        "extern" => extern_from_sexpr(&list[1..]),
        _ => call_var_from_sexpr(id, &list[1..]),
      },
      Some(fun_elem) => call_from_sexpr(fun_elem, &list[1..]),
      None => Err(format!("empty list is not an expression")),
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

fn cond_from_sexpr(elems: &[sexpr::Elem]) -> Result<Expr, String> {
  let mut arms = Vec::new();
  for elem in elems.iter() {
    match *elem {
      sexpr::Elem::List(ref arm_elems) => {
        if arm_elems.len() >= 1 {
          let arm_cond = try!(expr_from_sexpr(&arm_elems[0]));
          let arm_stmts = try!(stmts_from_sexprs(&arm_elems[1..]));
          arms.push((arm_cond, arm_stmts))
        } else {
          return Err(format!("'cond' arm list cannot be empty"));
        }
      },
      _ => return Err(format!("'cond' arm must be a list")),
    }
  }
  Ok(Expr::Cond(arms))
}

fn when_from_sexpr(elems: &[sexpr::Elem]) -> Result<Expr, String> {
  if elems.len() >= 1 {
    let cond = try!(expr_from_sexpr(&elems[0]));
    let body = try!(stmts_from_sexprs(&elems[1..]));
    Ok(Expr::When(box cond, body))
  } else {
    Err(format!("'when' cannot be empty"))
  }
}

fn unless_from_sexpr(elems: &[sexpr::Elem]) -> Result<Expr, String> {
  if elems.len() >= 1 {
    let cond = try!(expr_from_sexpr(&elems[0]));
    let body = try!(stmts_from_sexprs(&elems[1..]));
    Ok(Expr::Unless(box cond, body))
  } else {
    Err(format!("'unless' cannot be empty"))
  }
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

fn lambda_from_sexpr(elems: &[sexpr::Elem]) -> Result<Expr, String> {
  if elems.len() >= 1 {
    let args = try!(vars_from_sexpr(&elems[0]));
    let stmts = try!(stmts_from_sexprs(&elems[1..]));
    Ok(Expr::Lambda(args, stmts))
  } else {
    Err(format!("'lambda' expects at least a list of args"))
  }
}

fn extern_from_sexpr(elems: &[sexpr::Elem]) -> Result<Expr, String> {
  if elems.len() >= 1 {
    let name = try!(var_from_sexpr(&elems[0]));
    let args = try!(exprs_from_sexprs(&elems[1..]));
    Ok(Expr::Extern(name, args))
  } else {
    Err(format!("'extern' expects at least the extern name"))
  }
}

fn call_from_sexpr(head: &sexpr::Elem, elems: &[sexpr::Elem]) -> Result<Expr, String> {
  let fun = try!(expr_from_sexpr(head));
  let args = try!(exprs_from_sexprs(elems));
  Ok(Expr::Call(box fun, args))
}

fn call_var_from_sexpr(head: &str, elems: &[sexpr::Elem]) -> Result<Expr, String> {
  let fun = Expr::Var(Var(head.to_string()));
  let args = try!(exprs_from_sexprs(elems));
  Ok(Expr::Call(box fun, args))
}

fn mod_name_from_sexpr(elem: &sexpr::Elem) -> Result<ModName, String> {
  match *elem {
    sexpr::Elem::Identifier(ref name) => Ok(ModName(name.clone())),
    _ => Err(format!("mod name must be a string")),
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
    sexpr::Elem::List(ref elems) => vars_from_sexprs(&elems[..]),
    _ => Err(format!("expected a list of vars")),
  }
}

fn vars_from_sexprs(elems: &[sexpr::Elem]) -> Result<Vec<Var>, String> {
  let mut vars = Vec::new();
  for var in elems.iter() {
    vars.push(try!(var_from_sexpr(var)));
  }
  Ok(vars)
}

#[cfg(test)]
mod test {
  use sexpr;
  use spiral as s;

  fn parse_prog(txt: &str) -> s::Prog {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    sexpr::to_spiral::prog_from_sexpr(&sexpr).unwrap()
  }

  fn parse_mod(txt: &str) -> s::Mod {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    sexpr::to_spiral::mod_from_sexpr(&sexpr).unwrap()
  }

  fn parse_stmt(txt: &str) -> s::Stmt {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    sexpr::to_spiral::stmt_from_sexpr(&sexpr).unwrap()
  }

  fn parse_expr(txt: &str) -> s::Expr {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    sexpr::to_spiral::expr_from_sexpr(&sexpr).unwrap()
  }

  fn fun_def(id: &str, args: Vec<s::Var>, stmts: Vec<s::Stmt>) -> s::FunDef {
    s::FunDef { var: var(id), args: args, stmts: stmts }
  }

  fn var(id: &str) -> s::Var {
    s::Var(id.to_string())
  }

  fn mod_name(id: &str) -> s::ModName {
    s::ModName(id.to_string())
  }

  fn int_expr(num: i32) -> s::Expr {
    s::Expr::Int(num)
  }

  fn var_expr(id: &str) -> s::Expr {
    s::Expr::Var(var(id))
  }

  #[test]
  fn test_prog() {
    assert_eq!(parse_prog("(program (var x 10) (fun get-x () x))"),
      s::Prog { stmts: vec![
        s::Stmt::Var(var("x"), int_expr(10)),
        s::Stmt::Fun(fun_def("get-x", vec![], vec![
            s::Stmt::Expr(var_expr("x")),
          ])),
      ]});
  }

  #[test]
  fn test_mod() {
    assert_eq!(parse_mod("(module math (var zero 0) (export zero))"),
      s::Mod { name: mod_name("math"), decls: vec![
        s::Decl::Stmt(s::Stmt::Var(var("zero"), int_expr(0))),
        s::Decl::Export(vec![var("zero")]),
      ]});
  }

  #[test]
  fn test_fun_def() {
    assert_eq!(parse_stmt("(fun f (a b) 3)"),
      s::Stmt::Fun(fun_def("f", vec![var("a"), var("b")], vec![
          s::Stmt::Expr(int_expr(3))
        ])));
  }

  #[test]
  fn test_var_def() {
    assert_eq!(parse_stmt("(var x 3)"),
      s::Stmt::Var(var("x"), int_expr(3)));
  }

  #[test]
  fn test_module_import() {
    assert_eq!(parse_stmt("(import math io)"),
      s::Stmt::Import(vec![
        s::ImportDef::Mod(mod_name("math")),
        s::ImportDef::Mod(mod_name("io")),
      ]));
  }

  #[test]
  fn test_only_and_except_imports() {
    assert_eq!(parse_stmt("(import (only math + -))"),
      s::Stmt::Import(vec![s::ImportDef::Only(
          box s::ImportDef::Mod(mod_name("math")),
          vec![var("+"), var("-")])]));
    assert_eq!(parse_stmt("(import (except math / *))"),
      s::Stmt::Import(vec![s::ImportDef::Except(
          box s::ImportDef::Mod(mod_name("math")),
          vec![var("/"), var("*")])]));
  }

  #[test]
  fn test_prefix_import() {
    assert_eq!(parse_stmt("(import (prefix math m.))"),
      s::Stmt::Import(vec![s::ImportDef::Prefix(
          box s::ImportDef::Mod(mod_name("math")),
          var("m."))]));
  }

  #[test]
  fn test_if_expr() {
    assert_eq!(parse_expr("(if a 2 3)"),
      s::Expr::If(box var_expr("a"), box int_expr(2), box int_expr(3)));
  }

  #[test]
  fn test_cond_expr() {
    assert_eq!(parse_expr("(cond (a 1 2) (b 2))"),
      s::Expr::Cond(vec![
          (var_expr("a"), vec![
              s::Stmt::Expr(int_expr(1)),
              s::Stmt::Expr(int_expr(2)),
            ]),
          (var_expr("b"), vec![
              s::Stmt::Expr(int_expr(2)),
            ]),
        ])
      );

    assert_eq!(parse_expr("(cond (a 1 2) (b 2))"),
      s::Expr::Cond(vec![
          (var_expr("a"), vec![
              s::Stmt::Expr(int_expr(1)),
              s::Stmt::Expr(int_expr(2)),
            ]),
          (var_expr("b"), vec![
              s::Stmt::Expr(int_expr(2)),
            ]),
        ])
      );
  }

  #[test]
  fn test_when_expr() {
    assert_eq!(parse_expr("(when a 1 2)"),
      s::Expr::When(box var_expr("a"), vec![
          s::Stmt::Expr(int_expr(1)),
          s::Stmt::Expr(int_expr(2)),
        ]));
  }

  #[test]
  fn test_unless_expr() {
    assert_eq!(parse_expr("(unless b 1 2)"),
      s::Expr::Unless(box var_expr("b"), vec![
          s::Stmt::Expr(int_expr(1)),
          s::Stmt::Expr(int_expr(2)),
        ]));
  }

  #[test]
  fn test_do_expr() {
    assert_eq!(parse_expr("(do ((i 1 j) (j 0 i)) (x 0) 1 2)"),
      s::Expr::Do(
        vec![
          (var("i"), int_expr(1), var_expr("j")),
          (var("j"), int_expr(0), var_expr("i")),
        ],
        box var_expr("x"),
        vec![
          s::Stmt::Expr(int_expr(0)),
        ],
        vec![
          s::Stmt::Expr(int_expr(1)),
          s::Stmt::Expr(int_expr(2)),
        ]
      ));
  }

  #[test]
  fn test_and_expr() {
    assert_eq!(parse_expr("(and 1 2 3)"),
      s::Expr::And(vec![int_expr(1), int_expr(2), int_expr(3)]));
  }

  #[test]
  fn test_or_expr() {
    assert_eq!(parse_expr("(or 1 b 3)"),
      s::Expr::Or(vec![int_expr(1), var_expr("b"), int_expr(3)]));
  }

  #[test]
  fn test_begin_expr() {
    assert_eq!(parse_expr("(begin 1 2)"),
      s::Expr::Begin(vec![
        s::Stmt::Expr(int_expr(1)),
        s::Stmt::Expr(int_expr(2)),
      ]));
  }

  #[test]
  fn test_let_expr() {
    assert_eq!(parse_expr("(let ((a 1) (b 2)) a b)"),
      s::Expr::Let(vec![
          (var("a"), int_expr(1)),
          (var("b"), int_expr(2)),
        ], vec![
          s::Stmt::Expr(var_expr("a")),
          s::Stmt::Expr(var_expr("b")),
        ]));
  }

  #[test]
  fn test_call_expr() {
    assert_eq!(parse_expr("(+ 1 2)"),
      s::Expr::Call(box var_expr("+"), vec![int_expr(1), int_expr(2)]));
    assert_eq!(parse_expr("((f a) b c)"),
      s::Expr::Call(box s::Expr::Call(box var_expr("f"), vec![var_expr("a")]),
        vec![var_expr("b"), var_expr("c")]));
  }

  #[test]
  fn test_lambda_expr() {
    assert_eq!(parse_expr("(lambda (x y) (var sum (+ x y)) sum)"),
      s::Expr::Lambda(vec![var("x"), var("y")], vec![
        s::Stmt::Var(var("sum"), s::Expr::Call(box var_expr("+"), 
          vec![var_expr("x"), var_expr("y")])),
        s::Stmt::Expr(var_expr("sum"))
      ]));
  }

  #[test]
  fn test_extern() {
    assert_eq!(parse_expr("(extern blow_up 1 2 3)"),
      s::Expr::Extern(var("blow_up"), vec![int_expr(1), int_expr(2), int_expr(3)]));
  }

  #[test]
  fn test_string() {
    assert_eq!(parse_expr("\"befunge\""),
      s::Expr::String("befunge".to_string()));
  }
}
