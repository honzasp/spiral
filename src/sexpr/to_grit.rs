#![cfg(test)]
use sexpr;
use grit;

pub fn prog_from_sexpr(prog: &sexpr::Elem) -> Result<grit::ProgDef, String> {
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
                "double" => obj_defs.push(try!(double_def_from_sexprs(&def_list[1..]))),
                _ => return Err(format!("invalid def head")),
              },
              _ => return Err(format!("def must begin with an identifier")),
            },
          _ => return Err(format!("def must be a list")),
        }
      }

      Ok(grit::ProgDef {
        fun_defs: fun_defs,
        obj_defs: obj_defs,
        main_fun: main_fun,
      })
    },
    _ => Err(format!("program has to be a list")),
  }
}

pub fn fun_def_from_sexpr(elem: &sexpr::Elem) -> Result<grit::FunDef, String> {
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

fn fun_def_from_sexprs(list: &[sexpr::Elem]) -> Result<grit::FunDef, String> {
  if list.len() >= 5 {
    let name = try!(fun_name_from_sexpr(&list[0]));
    let capture_count = try!(int_from_sexpr(&list[1]));
    let arg_count = try!(int_from_sexpr(&list[2]));
    let var_count = try!(int_from_sexpr(&list[3]));
    let start = try!(label_from_sexpr(&list[4]));

    let mut blocks = Vec::new();
    for elem in list[5..].iter() {
      blocks.push(try!(block_from_sexpr(elem)));
    }

    Ok(grit::FunDef {
      name: name,
      capture_count: capture_count as usize,
      arg_count: arg_count as usize,
      var_count: var_count as usize,
      start_label: start,
      blocks: blocks,
    })
  } else {
    Err(format!("fun def must have 7 elems"))
  }
}

fn str_def_from_sexprs(list: &[sexpr::Elem]) -> Result<grit::ObjDef, String> {
  if list.len() == 2 {
    let name = try!(obj_name_from_sexpr(&list[0]));
    let bytes = match list[1] {
      sexpr::Elem::Int(byte) if byte >= 0 && byte < 256 => vec![byte as u8],
      sexpr::Elem::String(ref txt) => txt.clone().into_bytes(),
      _ => return Err(format!("string must be defined by bytes or by string")),
    };
    Ok(grit::ObjDef { name: name, obj: grit::Obj::String(bytes) })
  } else if list.len() >= 1 {
    let name = try!(obj_name_from_sexpr(&list[0]));
    let mut bytes = Vec::new();
    for elem in &list[1..] {
      match *elem {
        sexpr::Elem::Int(byte) if byte >= 0 && byte < 256 => bytes.push(byte as u8),
        _ => return Err(format!("string def must contain bytes")),
      }
    }
    Ok(grit::ObjDef { name: name, obj: grit::Obj::String(bytes) })
  } else {
    Err(format!("string def must have at least 2 elems"))
  }
}

fn double_def_from_sexprs(list: &[sexpr::Elem]) -> Result<grit::ObjDef, String> {
  if list.len() == 2 {
    let name = try!(obj_name_from_sexpr(&list[0]));
    let num = try!(match list[1] {
      sexpr::Elem::Double(num) => Ok(num),
      sexpr::Elem::Int(num) => Ok(num as f64),
      _ => Err(format!("invalid double value")),
    });
    Ok(grit::ObjDef { name: name, obj: grit::Obj::Double(num) })
  } else {
    Err(format!("double def must have 3 elems"))
  }
}

pub fn block_from_sexpr(elem: &sexpr::Elem) -> Result<grit::Block, String> {
  match *elem {
    sexpr::Elem::List(ref elems) => 
      if elems.len() >= 2 {
        let label = try!(label_from_sexpr(&elems[0]));
        let mut ops = Vec::new();
        for elem in elems[1..elems.len() - 1].iter() {
          ops.push(try!(op_from_sexpr(elem)));
        }
        let jump = try!(jump_from_sexpr(&elems[elems.len() - 1]));
        Ok(grit::Block { label: label, ops: ops, jump: jump })
      } else {
        Err(format!("block must have 3 elems"))
      },
    _ => Err(format!("block must be a list")),
  }
}

fn op_from_sexpr(elem: &sexpr::Elem) -> Result<grit::Op, String> {
  match *elem {
    sexpr::Elem::List(ref elems) => match elems.get(0) {
      Some(&sexpr::Elem::Identifier(ref head)) => match &head[..] {
        "call" => call_from_sexprs(&elems[1..]),
        "extern-call" => extern_call_from_sexprs(&elems[1..]),
        "alloc-clos" => alloc_clos_from_sexprs(&elems[1..]),
        "assign" => assign_from_sexprs(&elems[1..]),
        _ => Err(format!("unknown op")),
      },
      _ => Err(format!("invalid op head")),
    },
    _ => Err(format!("op must be a list")),
  }
}

fn call_from_sexprs(elems: &[sexpr::Elem]) -> Result<grit::Op, String> {
  if elems.len() >= 2 {
    let dst_var = try!(var_from_sexpr(&elems[0]));
    let callee = try!(callee_from_sexpr(&elems[1]));
    let args = try!(vals_from_sexprs(&elems[2..]));
    Ok(grit::Op::Call(dst_var, callee, args))
  } else {
    Err(format!("call must have at least 3 elems"))
  }
}

fn extern_call_from_sexprs(elems: &[sexpr::Elem]) -> Result<grit::Op, String> {
  if elems.len() >= 2 {
    let dst_var = try!(var_from_sexpr(&elems[0]));
    let extern_name = try!(extern_name_from_sexpr(&elems[1]));
    let args = try!(vals_from_sexprs(&elems[2..]));
    Ok(grit::Op::ExternCall(dst_var, extern_name, args))
  } else {
    Err(format!("extern-call must have at least 3 elems"))
  }
}

fn alloc_clos_from_sexprs(elems: &[sexpr::Elem]) -> Result<grit::Op, String> {
  let mut closs = Vec::new();
  for elem in elems.iter() {
    closs.push(try!(clos_from_sexpr(elem)));
  }
  Ok(grit::Op::AllocClos(closs))
}

fn clos_from_sexpr(elem: &sexpr::Elem) 
  -> Result<(grit::Var, grit::FunName, Vec<grit::Val>), String> 
{
  match *elem {
    sexpr::Elem::List(ref list) =>
      if list.len() >= 2 {
        let var = try!(var_from_sexpr(&list[0]));
        let name = try!(fun_name_from_sexpr(&list[1]));
        let captures = try!(vals_from_sexprs(&list[2..]));
        Ok((var, name, captures))
      } else {
        Err(format!("clos allocation must have at least 2 elems"))
      },
    _ => Err(format!("clos allocation must be a list")),
  }
}

fn assign_from_sexprs(elems: &[sexpr::Elem]) -> Result<grit::Op, String> {
  let mut assigns = Vec::new();
  for elem in elems.iter() {
    assigns.push(try!(var_val_from_sexpr(elem)));
  }
  Ok(grit::Op::Assign(assigns))
}

fn var_val_from_sexpr(elem: &sexpr::Elem) -> Result<(grit::Var, grit::Val), String> {
  match *elem {
    sexpr::Elem::List(ref list) =>
      if list.len() == 2 {
        let var = try!(var_from_sexpr(&list[0]));
        let val = try!(val_from_sexpr(&list[1]));
        Ok((var, val))
      } else {
        Err(format!("assignment must have 2 elems"))
      },
    _ => Err(format!("assignment must be a list")),
  }
}

fn jump_from_sexpr(elem: &sexpr::Elem) -> Result<grit::Jump, String> {
  match *elem {
    sexpr::Elem::List(ref elems) => match elems.get(0) {
      Some(&sexpr::Elem::Identifier(ref head)) => match &head[..] {
        "goto" => goto_from_sexprs(&elems[1..]),
        "return" => return_from_sexprs(&elems[1..]),
        "tail-call" => tail_call_from_sexprs(&elems[1..]),
        "branch" => branch_from_sexprs(&elems[1..]),
        _ => Err(format!("unknown jump")),
      },
      _ => Err(format!("invalid jump head")),
    },
    _ => Err(format!("jump must be a list")),
  }
}

fn goto_from_sexprs(elems: &[sexpr::Elem]) -> Result<grit::Jump, String> {
  if elems.len() == 1 {
    let label = try!(label_from_sexpr(&elems[0]));
    Ok(grit::Jump::Goto(label))
  } else {
    Err(format!("goto must have 2 elems"))
  }
}

fn return_from_sexprs(elems: &[sexpr::Elem]) -> Result<grit::Jump, String> {
  if elems.len() == 1 {
    let val = try!(val_from_sexpr(&elems[0]));
    Ok(grit::Jump::Return(val))
  } else {
    Err(format!("return must have 2 elems"))
  }
}

fn tail_call_from_sexprs(elems: &[sexpr::Elem]) -> Result<grit::Jump, String> {
  if elems.len() >= 1 {
    let callee = try!(callee_from_sexpr(&elems[0]));
    let args = try!(vals_from_sexprs(&elems[1..]));
    Ok(grit::Jump::TailCall(callee, args))
  } else {
    Err(format!("tail-call must have 3 elems"))
  }
}

fn branch_from_sexprs(elems: &[sexpr::Elem]) -> Result<grit::Jump, String> {
  if elems.len() == 3 {
    let boolval = try!(boolval_from_sexpr(&elems[0]));
    let then_label = try!(label_from_sexpr(&elems[1]));
    let else_label = try!(label_from_sexpr(&elems[2]));
    Ok(grit::Jump::Branch(boolval, then_label, else_label))
  } else {
    Err(format!("branch must have 4 elems"))
  }
}

fn callee_from_sexpr(elem: &sexpr::Elem) -> Result<grit::Callee, String> {
  fn combinator_from_sexprs(elems: &[sexpr::Elem]) -> Result<grit::Callee, String> {
    if elems.len() == 1 {
      Ok(grit::Callee::Combinator(try!(fun_name_from_sexpr(&elems[0]))))
    } else {
      Err(format!("combinator callee must have 2 elems"))
    }
  }

  fn known_closure_from_sexprs(elems: &[sexpr::Elem]) -> Result<grit::Callee, String> {
    if elems.len() == 2 {
      Ok(grit::Callee::KnownClosure(
          try!(fun_name_from_sexpr(&elems[0])),
          try!(val_from_sexpr(&elems[1]))))
    } else {
      Err(format!("known-closure callee must have 3 elems"))
    }
  }

  fn unknown_from_sexprs(elems: &[sexpr::Elem]) -> Result<grit::Callee, String> {
    if elems.len() == 1 {
      Ok(grit::Callee::Unknown(try!(val_from_sexpr(&elems[0]))))
    } else {
      Err(format!("unknown callee must have 2 elems"))
    }
  }

  match *elem {
    sexpr::Elem::List(ref elems) => match elems.get(0) {
      Some(&sexpr::Elem::Identifier(ref head)) => match &head[..] {
        "combinator" => combinator_from_sexprs(&elems[1..]),
        "known-closure" => known_closure_from_sexprs(&elems[1..]),
        "unknown" => unknown_from_sexprs(&elems[1..]),
        _ => Err(format!("unknown callee")),
      },
      _ => Err(format!("bad callee head")),
    },
    _ => Err(format!("callee must be a list")),
  }
}

fn vals_from_sexprs(elems: &[sexpr::Elem]) -> Result<Vec<grit::Val>, String> {
  let mut vals = Vec::new();
  for elem in elems.iter() {
    vals.push(try!(val_from_sexpr(elem)));
  }
  Ok(vals)
}

fn val_from_sexpr(elem: &sexpr::Elem) -> Result<grit::Val, String> {
  match *elem {
    sexpr::Elem::List(ref elems) => match elems.get(0) {
      Some(&sexpr::Elem::Identifier(ref head)) => match &head[..] {
        "var" if elems.len() == 2 =>
          Ok(grit::Val::Var(try!(var_from_sexpr(&elems[1])))),
        "arg" if elems.len() == 2 =>
          Ok(grit::Val::Arg(try!(int_from_sexpr(&elems[1])) as usize)),
        "capture" if elems.len() == 2 =>
          Ok(grit::Val::Capture(try!(int_from_sexpr(&elems[1])) as usize)),
        "combinator" if elems.len() == 2 =>
          Ok(grit::Val::Combinator(try!(fun_name_from_sexpr(&elems[1])))),
        "obj" if elems.len() == 2 =>
          Ok(grit::Val::Obj(try!(obj_name_from_sexpr(&elems[1])))),
        "int" if elems.len() == 2 =>
          Ok(grit::Val::Int(try!(int_from_sexpr(&elems[1])))),
        "true" if elems.len() == 1 => Ok(grit::Val::True),
        "false" if elems.len() == 1 => Ok(grit::Val::False),
        "undefined" if elems.len() == 1 => Ok(grit::Val::Undefined),
        _ => Err(format!("invalid val")),
      },
      _ => Err(format!("invalid val head")),
    },
    _ => Err(format!("val must be a list")),
  }
}

fn boolval_from_sexpr(elem: &sexpr::Elem) -> Result<grit::Boolval, String> {
  match *elem {
    sexpr::Elem::List(ref elems) => match elems.get(0) {
      Some(&sexpr::Elem::Identifier(ref head)) => match &head[..] {
        "is-true" if elems.len() == 2 =>
          Ok(grit::Boolval::IsTrue(try!(val_from_sexpr(&elems[1])))),
        "is-false" if elems.len() == 2 =>
          Ok(grit::Boolval::IsFalse(try!(val_from_sexpr(&elems[1])))),
        _ => Err(format!("invalid boolval")),
      },
      _ => Err(format!("invalid boolval head")),
    },
    _ => Err(format!("boolval must be a string")),
  }
}

fn var_from_sexpr(elem: &sexpr::Elem) -> Result<grit::Var, String> {
  match *elem {
    sexpr::Elem::Int(i) =>
      Ok(grit::Var(i as usize)),
    _ => Err(format!("expected a variable")),
  }
}

fn int_from_sexpr(elem: &sexpr::Elem) -> Result<i32, String> {
  match *elem {
    sexpr::Elem::Int(i) => Ok(i),
    _ => Err(format!("expected an int")),
  }
}


fn fun_name_from_sexpr(elem: &sexpr::Elem) -> Result<grit::FunName, String> {
  match *elem {
    sexpr::Elem::Identifier(ref id) =>
      Ok(grit::FunName(id.clone())),
    _ => Err(format!("expected a fun name")),
  }
}

fn label_from_sexpr(elem: &sexpr::Elem) -> Result<grit::Label, String> {
  match *elem {
    sexpr::Elem::Identifier(ref id) =>
      Ok(grit::Label(id.clone())),
    _ => Err(format!("expected a label")),
  }
}

fn extern_name_from_sexpr(elem: &sexpr::Elem) -> Result<grit::ExternName, String> {
  match *elem {
    sexpr::Elem::Identifier(ref id) =>
      Ok(grit::ExternName(id.clone())),
    _ => Err(format!("expected an extern name")),
  }
}

fn obj_name_from_sexpr(elem: &sexpr::Elem) -> Result<grit::ObjName, String> {
  match *elem {
    sexpr::Elem::Identifier(ref id) =>
      Ok(grit::ObjName(id.clone())),
    _ => Err(format!("expected an object name")),
  }
}

#[cfg(test)]
mod test {
  use sexpr;
  use grit::*;

  fn parse_prog(txt: &str) -> ProgDef {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    sexpr::to_grit::prog_from_sexpr(&sexpr).unwrap()
  }

  fn parse_fun_def(txt: &str) -> FunDef {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    sexpr::to_grit::fun_def_from_sexpr(&sexpr).unwrap()
  }

  fn parse_block(txt: &str) -> Block {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    sexpr::to_grit::block_from_sexpr(&sexpr).unwrap()
  }

  fn var(i: usize) -> Var { Var(i) }
  fn label(id: &str) -> Label { Label(id.to_string()) }
  fn obj_name(id: &str) -> ObjName { ObjName(id.to_string()) }
  fn fun(id: &str) -> FunName { FunName(id.to_string()) }
  fn extern_name(id: &str) -> ExternName { ExternName(id.to_string()) }
  fn byte_str(txt: &str) -> Vec<u8> { txt.to_string().into_bytes() }

  #[test]
  fn test_empty_prog() {
    assert_eq!(parse_prog("(program main)"),
      ProgDef {
        main_fun: fun("main"),
        fun_defs: vec![],
        obj_defs: vec![],
      });
  }

  #[test]
  fn test_prog_with_fun() {
    assert_eq!(parse_prog("(program main (fun main 1 2 3 start))"),
      ProgDef {
        main_fun: fun("main"),
        fun_defs: vec![
          FunDef { name: fun("main"),
            capture_count: 1,
            arg_count: 2,
            var_count: 3,
            start_label: label("start"),
            blocks: vec![],
          },
        ],
        obj_defs: vec![],
      });
  }

  #[test]
  fn test_prog_with_strings() {
    assert_eq!(parse_prog("(program main
      (string s1 \"spiral\")
      (string s2 1 2 3 4)
      (string s3 1)
      (string s4))"),
      ProgDef {
        main_fun: fun("main"),
        fun_defs: vec![],
        obj_defs: vec![
          ObjDef { name: obj_name("s1"), obj: Obj::String(byte_str("spiral")) },
          ObjDef { name: obj_name("s2"), obj: Obj::String(vec![1,2,3,4]) },
          ObjDef { name: obj_name("s3"), obj: Obj::String(vec![1]) },
          ObjDef { name: obj_name("s4"), obj: Obj::String(vec![]) },
        ],
      });
  }

  #[test]
  fn test_prog_with_double() {
    assert_eq!(parse_prog("(program main (double half 0.5))"),
      ProgDef {
        main_fun: fun("main"),
        fun_defs: vec![],
        obj_defs: vec![
          ObjDef { name: obj_name("half"), obj: Obj::Double(0.5) },
        ],
      });
  }

  #[test]
  fn test_fun_def() {
    assert_eq!(parse_fun_def("(fun ff 1 2 3 start
        (start
          (assign (0 (int 10)))
          (goto end))
        (end
          (return (int 100))))"),
      FunDef { name: fun("ff"),
        capture_count: 1,
        arg_count: 2,
        var_count: 3,
        start_label: label("start"),
        blocks: vec![
          Block { label: label("start"), ops: vec![
              Op::Assign(vec![(var(0), Val::Int(10))]),
            ], jump: Jump::Goto(label("end")) },
          Block { label: label("end"), ops: vec![
            ], jump: Jump::Return(Val::Int(100)) },
        ]});
  }

  #[test]
  fn test_assign() {
    assert_eq!(parse_block("(bb
        (assign)
        (assign (1 (int 10)) (2 (int 20)) (3 (int 30)))
        (return (int 1)))"),
      Block { label: label("bb"), ops: vec![
          Op::Assign(vec![]),
          Op::Assign(vec![
            (var(1), Val::Int(10)),
            (var(2), Val::Int(20)),
            (var(3), Val::Int(30)),
          ]),
        ], jump: Jump::Return(Val::Int(1)) });
  }
        
  #[test]
  fn test_calls() {
    assert_eq!(parse_block("(bb
        (call 1 (combinator f) (int 1))
        (call 2 (combinator g))
        (extern-call 3 __boom__ (int 2) (int 3))
        (extern-call 4 __boom__)
        (return (int 1)))"),
      Block { label: label("bb"), ops: vec![
          Op::Call(var(1), Callee::Combinator(fun("f")), vec![Val::Int(1)]),
          Op::Call(var(2), Callee::Combinator(fun("g")), vec![]),
          Op::ExternCall(var(3), extern_name("__boom__"), vec![Val::Int(2), Val::Int(3)]),
          Op::ExternCall(var(4), extern_name("__boom__"), vec![]),
        ], jump: Jump::Return(Val::Int(1)) });
  }

  #[test]
  fn test_callees() {
    assert_eq!(parse_block("(bb
        (call 1 (combinator f) (int 1))
        (call 2 (known-closure f (var 0)) (int 2))
        (call 3 (unknown (var 1)) (int 3))
        (return (int 1)))"),
      Block { label: label("bb"), ops: vec![
          Op::Call(var(1), Callee::Combinator(fun("f")), vec![Val::Int(1)]),
          Op::Call(var(2), Callee::KnownClosure(fun("f"), Val::Var(var(0))),
            vec![Val::Int(2)]),
          Op::Call(var(3), Callee::Unknown(Val::Var(var(1))), vec![Val::Int(3)]),
        ], jump: Jump::Return(Val::Int(1)) });
  }
        
  #[test]
  fn test_alloc_clos() {
    assert_eq!(parse_block("(bb
        (alloc-clos (0 ff (int 1) (int 2)) (1 gg (int 3)))
        (return (int 1)))"),
      Block { label: label("bb"), ops: vec![
          Op::AllocClos(vec![
            (var(0), fun("ff"), vec![Val::Int(1), Val::Int(2)]),
            (var(1), fun("gg"), vec![Val::Int(3)]),
          ])
        ], jump: Jump::Return(Val::Int(1)) });
  }
        
  #[test]
  fn test_goto() {
    assert_eq!(parse_block("(bb (goto bb-2))"),
      Block { label: label("bb"), ops: vec![],
        jump: Jump::Goto(label("bb-2")) });
  }

  #[test]
  fn test_return() {
    assert_eq!(parse_block("(bb (return (int 60)))"),
      Block { label: label("bb"), ops: vec![],
        jump: Jump::Return(Val::Int(60)) });
  }

  #[test]
  fn test_tail_call() {
    assert_eq!(parse_block("(bb (tail-call (combinator ff) (int 1) (int 2)))"),
      Block { label: label("bb"), ops: vec![],
        jump: Jump::TailCall(Callee::Combinator(fun("ff")),
          vec![Val::Int(1), Val::Int(2)]) });
  }

  #[test]
  fn test_branch() {
    assert_eq!(parse_block("(bb (branch (is-true (int 1)) bb-2 bb-3))"),
      Block { label: label("bb"), ops: vec![],
        jump: Jump::Branch(Boolval::IsTrue(Val::Int(1)), label("bb-2"), label("bb-3")) });
    assert_eq!(parse_block("(bb (branch (is-false (int 1)) bb-2 bb-3))"),
      Block { label: label("bb"), ops: vec![],
        jump: Jump::Branch(Boolval::IsFalse(Val::Int(1)), label("bb-2"), label("bb-3")) });
  }

  #[test]
  fn test_vals() {
    assert_eq!(parse_block("(bb
      (tail-call (combinator ff)
        (var 1)
        (arg 2)
        (capture 3)
        (combinator ff)
        (obj his-object)
        (int 100)
        (true) (false) (undefined)))"),
      Block { label: label("bb"), ops: vec![], 
        jump: Jump::TailCall(Callee::Combinator(fun("ff")), vec![
          Val::Var(var(1)),
          Val::Arg(2),
          Val::Capture(3),
          Val::Combinator(fun("ff")),
          Val::Obj(obj_name("his-object")),
          Val::Int(100),
          Val::True, Val::False, Val::Undefined,
        ])});
  }
}
