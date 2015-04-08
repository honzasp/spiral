use std::collections::{HashSet};
use grit;

pub fn optimize(mut prog: grit::ProgDef) -> grit::ProgDef {
  let info = collect_prog_info(&prog);
  prog.fun_defs = prog.fun_defs.into_iter().filter(|def| {
      info.used_funs.contains(&def.name)
    }).collect();
  prog.obj_defs = prog.obj_defs.into_iter().filter(|def| {
      info.used_objs.contains(&def.name)
    }).collect();
  prog
}

struct ProgInfo {
  used_funs: HashSet<grit::FunName>,
  used_objs: HashSet<grit::ObjName>,
}

fn collect_prog_info(prog: &grit::ProgDef) -> ProgInfo {
  let mut info = ProgInfo {
    used_funs: HashSet::new(),
    used_objs: HashSet::new(),
  };
  info.used_funs.insert(prog.main_fun.clone());
  let mut collected_funs = HashSet::new();

  while info.used_funs.len() > collected_funs.len() {
    for fun_def in prog.fun_defs.iter() {
      let is_used = info.used_funs.contains(&fun_def.name);
      let is_collected = collected_funs.contains(&fun_def.name);
      if is_used && !is_collected {
        collect_fun(&mut info, fun_def);
        collected_funs.insert(fun_def.name.clone());
      }
    }
  }

  info
}

fn collect_fun(info: &mut ProgInfo, fun_def: &grit::FunDef) {
  for block in fun_def.blocks.iter() {
    for op in block.ops.iter() {
      collect_op(info, op);
    }
    collect_jump(info, &block.jump);
  }
}

fn collect_op(info: &mut ProgInfo, op: &grit::Op) {
  match *op {
    grit::Op::Call(_, ref callee, ref args) => {
      collect_callee(info, callee);
      for arg in args.iter() {
        collect_val(info, arg);
      }
    },
    grit::Op::ExternCall(_, _, ref args) =>
      for arg in args.iter() {
        collect_val(info, arg);
      },
    grit::Op::AllocClos(ref closs) =>
      for &(_, ref clos_name, ref captures) in closs.iter() {
        info.used_funs.insert(clos_name.clone());
        for capture in captures.iter() {
          collect_val(info, capture);
        }
      },
    grit::Op::Assign(ref var_vals) =>
      for &(_, ref val) in var_vals.iter() {
        collect_val(info, val);
      },
  }
}

fn collect_jump(info: &mut ProgInfo, jump: &grit::Jump) {
  match *jump {
    grit::Jump::Goto(_) => {},
    grit::Jump::TailCall(ref callee, ref args) => {
      collect_callee(info, callee);
      for arg in args.iter() {
        collect_val(info, arg);
      }
    },
    grit::Jump::Return(ref val) |
    grit::Jump::Branch(grit::Boolval::IsTrue(ref val), _, _) |
    grit::Jump::Branch(grit::Boolval::IsFalse(ref val), _, _) =>
      collect_val(info, val),
  }
}

fn collect_val(info: &mut ProgInfo, val: &grit::Val) {
  match *val {
    grit::Val::Combinator(ref fun_name) => {
      info.used_funs.insert(fun_name.clone());
    },
    grit::Val::Obj(ref obj_name) => {
      info.used_objs.insert(obj_name.clone());
    },
    grit::Val::Var(_) |
    grit::Val::Arg(_) |
    grit::Val::Capture(_) |
    grit::Val::Int(_) |
    grit::Val::True |
    grit::Val::False |
    grit::Val::Undefined => (),
  }
}

fn collect_callee(info: &mut ProgInfo, callee: &grit::Callee) {
  match *callee {
    grit::Callee::Combinator(ref fun_name) => {
      info.used_funs.insert(fun_name.clone());
    },
    grit::Callee::KnownClosure(ref fun_name, ref val) => {
      info.used_funs.insert(fun_name.clone());
      collect_val(info, val);
    },
    grit::Callee::Unknown(ref val) =>
      collect_val(info, val),
  }
}
