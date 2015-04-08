use grit;
use sexpr;

pub fn prog_to_sexpr(prog: &grit::ProgDef) -> sexpr::Elem {
  let mut list = vec![
    ident("program"),
    fun_name_to_sexpr(&prog.main_fun),
  ];
  list.extend(prog.fun_defs.iter().map(fun_def_to_sexpr));
  list.extend(prog.obj_defs.iter().map(obj_def_to_sexpr));
  sexpr::Elem::List(list)
}

pub fn fun_def_to_sexpr(def: &grit::FunDef) -> sexpr::Elem {
  let mut list = vec![
    ident("fun"),
    fun_name_to_sexpr(&def.name),
    sexpr::Elem::Int(def.capture_count as i32),
    sexpr::Elem::Int(def.arg_count as i32),
    sexpr::Elem::Int(def.var_count as i32),
    label_to_sexpr(&def.start_label),
  ];
  list.extend(def.blocks.iter().map(block_to_sexpr));
  sexpr::Elem::List(list)
}

pub fn obj_def_to_sexpr(def: &grit::ObjDef) -> sexpr::Elem {
  use std::{str};
  sexpr::Elem::List(match def.obj {
    grit::Obj::String(ref bytes) => {
      let mut list = vec![ident("string"), obj_name_to_sexpr(&def.name)];
      let body = match str::from_utf8(&bytes) {
        Ok(txt) => vec![sexpr::Elem::String(txt.to_string())],
        Err(_) => bytes.iter().map(|&b| sexpr::Elem::Int(b as i32)).collect(),
      };
      list.extend(body.into_iter());
      list
    },
    grit::Obj::Double(number) => vec![
      ident("double"),
      obj_name_to_sexpr(&def.name),
      sexpr::Elem::Double(number)
    ],
  })
}

pub fn block_to_sexpr(block: &grit::Block) -> sexpr::Elem {
  let mut list = vec![label_to_sexpr(&block.label)];
  list.extend(block.ops.iter().map(op_to_sexpr));
  list.push(jump_to_sexpr(&block.jump));
  sexpr::Elem::List(list)
}

pub fn op_to_sexpr(op: &grit::Op) -> sexpr::Elem {
  let mut list;
  match *op {
    grit::Op::Call(ref ret_var, ref callee, ref vals) => {
      list = vec![ident("call"), var_to_sexpr(ret_var), callee_to_sexpr(callee)];
      list.extend(vals.iter().map(val_to_sexpr));
    },
    grit::Op::ExternCall(ref ret_var, ref extern_name, ref vals) => {
      list = vec![ident("extern-call"), var_to_sexpr(ret_var),
        extern_name_to_sexpr(extern_name)];
      list.extend(vals.iter().map(val_to_sexpr));
    },
    grit::Op::AllocClos(ref var_closs) => {
      list = vec![ident("alloc-clos")];
      list.extend(var_closs.iter().map(|&(ref var, ref fun_name, ref captures)| {
          let mut list =vec![var_to_sexpr(var), fun_name_to_sexpr(fun_name)];
          list.extend(captures.iter().map(val_to_sexpr));
          sexpr::Elem::List(list)
        }));
    },
    grit::Op::Assign(ref assigns) => {
      list = vec![ident("assign")];
      list.extend(assigns.iter().map(|&(ref var, ref val)| {
          sexpr::Elem::List(vec![var_to_sexpr(var), val_to_sexpr(val)])
        }));
    },
  }
  sexpr::Elem::List(list)
}

pub fn jump_to_sexpr(jump: &grit::Jump) -> sexpr::Elem {
  sexpr::Elem::List(match *jump {
    grit::Jump::Goto(ref label) => vec![
      ident("goto"),
      label_to_sexpr(label),
    ],
    grit::Jump::Return(ref val) => vec![
      ident("return"),
      val_to_sexpr(val),
    ],
    grit::Jump::TailCall(ref fun_name, ref args) => {
      let mut list = vec![ident("tail-call"), callee_to_sexpr(fun_name)];
      list.extend(args.iter().map(val_to_sexpr));
      list
    },
    grit::Jump::Branch(ref boolval, ref then_label, ref else_label) => vec![
      ident("branch"),
      boolval_to_sexpr(boolval),
      label_to_sexpr(then_label),
      label_to_sexpr(else_label),
    ],
  })
}

pub fn val_to_sexpr(val: &grit::Val) -> sexpr::Elem {
  sexpr::Elem::List(match *val {
    grit::Val::Var(ref var) => 
      vec![ident("var"), var_to_sexpr(var)],
    grit::Val::Arg(idx) => 
      vec![ident("arg"), sexpr::Elem::Int(idx as i32)],
    grit::Val::Capture(idx) => 
      vec![ident("capture"), sexpr::Elem::Int(idx as i32)],
    grit::Val::Combinator(ref fun_name) => 
      vec![ident("combinator"), fun_name_to_sexpr(fun_name)],
    grit::Val::Obj(ref obj_name) => 
      vec![ident("obj"), obj_name_to_sexpr(obj_name)],
    grit::Val::Int(num) =>
      vec![ident("int"), sexpr::Elem::Int(num)],
    grit::Val::True =>
      vec![ident("true")],
    grit::Val::False =>
      vec![ident("false")],
    grit::Val::Undefined =>
      vec![ident("undefined")],
  })
}

pub fn boolval_to_sexpr(boolval: &grit::Boolval) -> sexpr::Elem {
  sexpr::Elem::List(match *boolval {
    grit::Boolval::IsTrue(ref val) => vec![ident("is-true"), val_to_sexpr(val)],
    grit::Boolval::IsFalse(ref val) => vec![ident("is-false"), val_to_sexpr(val)],
  })
}

pub fn callee_to_sexpr(callee: &grit::Callee) -> sexpr::Elem {
  sexpr::Elem::List(match *callee {
    grit::Callee::Combinator(ref fun_name) => 
      vec![ident("combinator"), fun_name_to_sexpr(fun_name)],
    grit::Callee::KnownClosure(ref fun_name, ref val) =>
      vec![ident("known-closure"), fun_name_to_sexpr(fun_name), val_to_sexpr(val)],
    grit::Callee::Unknown(ref val) =>
      vec![ident("unknown"), val_to_sexpr(val)],
  })
}

pub fn var_to_sexpr(var: &grit::Var) -> sexpr::Elem { sexpr::Elem::Int(var.0 as i32) }
pub fn fun_name_to_sexpr(i: &grit::FunName) -> sexpr::Elem { ident(&i.0[..]) }
pub fn extern_name_to_sexpr(i: &grit::ExternName) -> sexpr::Elem { ident(&i.0[..]) }
pub fn obj_name_to_sexpr(i: &grit::ObjName) -> sexpr::Elem { ident(&i.0[..]) }
pub fn label_to_sexpr(i: &grit::Label) -> sexpr::Elem { ident(&i.0[..]) }

fn ident(id: &str) -> sexpr::Elem {
  sexpr::Elem::Identifier(id.to_string())
}

#[cfg(test)]
mod test {
  use grit;
  use sexpr;

  fn check_identity(txt: &str) {
    let sexpr_1 = sexpr::parse::parse_sexpr(txt).unwrap();
    let grit_1 = sexpr::to_grit::prog_from_sexpr(&sexpr_1).unwrap();
    let sexpr_2 = grit::to_sexpr::prog_to_sexpr(&grit_1);
    assert_eq!(sexpr_1, sexpr_2);
  }

  #[test]
  fn test_empty_prog() {
    check_identity("(program start)");
  }

  #[test]
  fn test_prog_with_fun() {
    check_identity("(program start (fun ff 0 1 2 start))");
  }

  #[test]
  fn test_objs() {
    check_identity("(program main
      (string s1 \"foo and bar\")
      (string s2 0 1 2 255)
      (double pi 3.14)
      (double hundred 100.0))");
  }

  #[test]
  fn test_blocks() {
    check_identity("(program main
      (fun ff 0 1 2 start
        (start
          (assign (0 (int 10)))
          (goto step-1))
        (step-1
          (assign (1 (int 20)))
          (goto step-2))
        (step-2
          (goto step-3))
        (step-3
          (return (int 0)))))");
  }

  #[test]
  fn test_ops() {
    check_identity("(program main
      (fun ff 10 20 30 start
        (start
          (assign (0 (int 1)) (3 (int 2)))
          (assign)
          (call 2 (combinator ff) (int 1) (int 2))
          (extern-call 3 extern_fun (int 3))
          (alloc-clos (0 f1) (2 f2 (int 4) (int 5)))
          (return (int 1)))))");
  }

  #[test]
  fn test_jumps() {
    check_identity("(program main
      (fun ff 0 0 0 start
        (bb-1 (goto bb))
        (bb-2 (return (int 0)))
        (bb-3 (tail-call (combinator ff) (int 1) (int 2)))
        (bb-4 (branch (is-true (int 3)) on-true on-false))
        (bb-4 (branch (is-false (int 3)) on-false on-true))))");
  }

  #[test]
  fn test_callees() {
    check_identity("(program main
      (fun ff 0 0 0 start
        (bb-1 (call 0 (combinator ff))
              (call 1 (known-closure gg (var 0)))
              (call 2 (unknown (var 1)))
              (return (int 0)))))");
  }

  #[test]
  fn test_values() {
    check_identity("(program main
      (fun ff 2 4 5 start
        (bb-1 (tail-call (combinator ff)
          (var 0)
          (arg 1)
          (capture 2)
          (combinator x)
          (obj y)
          (int 100)
          (true)
          (false)
          (undefined)))))");
  }
}
