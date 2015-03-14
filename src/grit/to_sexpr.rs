use grit;
use sexpr;

pub fn prog_to_sexpr(prog: &grit::ProgDef) -> sexpr::Elem {
  let mut list = vec![
    ident("program"),
    fun_name_to_sexpr(&prog.main_fun),
  ];
  list.extend(prog.fun_defs.iter().map(fun_def_to_sexpr));
  sexpr::Elem::List(list)
}

pub fn fun_def_to_sexpr(def: &grit::FunDef) -> sexpr::Elem {
  sexpr::Elem::List(vec![
    fun_name_to_sexpr(&def.name),
    sexpr::Elem::Int(def.arg_count as i32),
    sexpr::Elem::Int(def.slot_count as i32),
    label_to_sexpr(&def.start_label),
    sexpr::Elem::List(def.blocks.iter().map(block_to_sexpr).collect()),
  ])
}

pub fn block_to_sexpr(block: &grit::Block) -> sexpr::Elem {
  sexpr::Elem::List(vec![
    label_to_sexpr(&block.label),
    sexpr::Elem::List(block.ops.iter().map(op_to_sexpr).collect()),
    jump_to_sexpr(&block.jump),
  ])
}

pub fn op_to_sexpr(op: &grit::Op) -> sexpr::Elem {
  sexpr::Elem::List(match *op {
    grit::Op::Call(ref ret_slot, ref fun_name, ref vals) => vec![
      ident("call"),
      slot_to_sexpr(ret_slot),
      fun_name_to_sexpr(fun_name),
      sexpr::Elem::List(vals.iter().map(val_to_sexpr).collect()),
    ],
    grit::Op::ExternCall(ref ret_slot, ref extern_name, ref vals) => vec![
      ident("extern-call"),
      slot_to_sexpr(ret_slot),
      extern_name_to_sexpr(extern_name),
      sexpr::Elem::List(vals.iter().map(val_to_sexpr).collect()),
    ],
    grit::Op::Assign(ref assigns) => vec![
      ident("assign"),
      sexpr::Elem::List(assigns.iter().map(|&(ref slot, ref val)| {
          sexpr::Elem::List(vec![slot_to_sexpr(slot), val_to_sexpr(val)])
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
      fun_name_to_sexpr(fun_name),
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
  match *val {
    grit::Val::Slot(ref slot) => slot_to_sexpr(slot),
    grit::Val::Int(num) => sexpr::Elem::Int(num),
    grit::Val::True => ident("true"),
    grit::Val::False => ident("false"),
  }
}

pub fn boolval_to_sexpr(boolval: &grit::Boolval) -> sexpr::Elem {
  sexpr::Elem::List(match *boolval {
    grit::Boolval::IsTrue(ref val) => vec![ident("is-true"), val_to_sexpr(val)],
    grit::Boolval::IsFalse(ref val) => vec![ident("is-false"), val_to_sexpr(val)],
  })
}

pub fn slot_to_sexpr(slot: &grit::Slot) -> sexpr::Elem {
  sexpr::Elem::List(vec![ident("slot"), sexpr::Elem::Int(slot.0 as i32)])
}

pub fn fun_name_to_sexpr(i: &grit::FunName) -> sexpr::Elem { ident(&i.0[..]) }
pub fn extern_name_to_sexpr(i: &grit::ExternName) -> sexpr::Elem { ident(&i.0[..]) }
pub fn label_to_sexpr(i: &grit::Label) -> sexpr::Elem { ident(&i.0[..]) }

fn ident(id: &str) -> sexpr::Elem {
  sexpr::Elem::Identifier(id.to_string())
}
