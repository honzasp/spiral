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
  sexpr::Elem::List(match def.obj {
    grit::Obj::String(ref bytes) => {
      let mut list = vec![ident("string"), obj_name_to_sexpr(&def.name)];
      list.extend(bytes.iter().map(|&b| sexpr::Elem::Int(b as i32)));
      list
    },
  })
}

pub fn block_to_sexpr(block: &grit::Block) -> sexpr::Elem {
  let mut list = vec![label_to_sexpr(&block.label)];
  list.extend(block.ops.iter().map(op_to_sexpr));
  list.push(jump_to_sexpr(&block.jump));
  sexpr::Elem::List(list)
}

pub fn op_to_sexpr(op: &grit::Op) -> sexpr::Elem {
  sexpr::Elem::List(match *op {
    grit::Op::Call(ref ret_var, ref callee, ref vals) => vec![
      ident("call"),
      var_to_sexpr(ret_var),
      callee_to_sexpr(callee),
      sexpr::Elem::List(vals.iter().map(val_to_sexpr).collect()),
    ],
    grit::Op::ExternCall(ref ret_var, ref extern_name, ref vals) => vec![
      ident("extern-call"),
      var_to_sexpr(ret_var),
      extern_name_to_sexpr(extern_name),
      sexpr::Elem::List(vals.iter().map(val_to_sexpr).collect()),
    ],
    grit::Op::AllocClos(ref var_closs) => vec![
      ident("alloc-clos"),
      sexpr::Elem::List(var_closs.iter().map(|&(ref var, ref fun_name, ref captures)| {
          sexpr::Elem::List(vec![
            var_to_sexpr(var),
            fun_name_to_sexpr(fun_name),
            sexpr::Elem::List(captures.iter().map(val_to_sexpr).collect()),
          ])
        }).collect()),
    ],
    grit::Op::Assign(ref assigns) => vec![
      ident("assign"),
      sexpr::Elem::List(assigns.iter().map(|&(ref var, ref val)| {
          sexpr::Elem::List(vec![var_to_sexpr(var), val_to_sexpr(val)])
        }).collect()),
    ],
  })
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
    grit::Jump::TailCall(ref fun_name, ref args) => vec![
      ident("tail-call"),
      callee_to_sexpr(fun_name),
      sexpr::Elem::List(args.iter().map(val_to_sexpr).collect()),
    ],
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
