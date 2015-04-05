use std::collections::{BitSet, HashMap};
use grit;

pub fn optimize(prog: grit::ProgDef) -> grit::ProgDef {
  let mut prog = prog;
  let prog_info = collect_prog_info(&prog);
  prog.fun_defs = prog.fun_defs.map_in_place(|mut fun_def| {
    let fun_info = prog_info.get(&fun_def.name).unwrap();
    fun_def.capture_count = fun_info.used_captures.len();
    fun_def.blocks = fun_def.blocks.map_in_place(|mut block| {
      block.ops = block.ops.map_in_place(|op| 
        optimize_op(&prog_info, fun_info, op));
      block.jump = optimize_jump(&prog_info, fun_info, block.jump);
      block
    });
    fun_def
  });
  prog
}

type ProgInfo = HashMap<grit::FunName, FunInfo>;

#[derive(Debug, PartialEq, Clone)]
struct FunInfo {
  used_captures: BitSet,
  used_vars: BitSet,
}

fn optimize_op(prog_info: &ProgInfo, fun_info: &FunInfo, op: grit::Op) -> grit::Op {
  match op {
    grit::Op::Call(var, callee, args) => 
      grit::Op::Call(var, optimize_callee(prog_info, fun_info, callee),
         args.map_in_place(|a| optimize_val(prog_info, fun_info, a))),
    grit::Op::ExternCall(var, extern_name, args) =>
      grit::Op::ExternCall(var, extern_name,
         args.map_in_place(|a| optimize_val(prog_info, fun_info, a))),
    grit::Op::AllocClos(closs) =>
      grit::Op::AllocClos(closs.into_iter().filter_map(|(var, closure_name, captures)| {
        if fun_info.used_vars.contains(&var.0) {
          let used_captures = &prog_info.get(&closure_name).unwrap().used_captures;
          let filtered = captures.into_iter().enumerate()
            .filter_map(|(idx, capture)| {
              if used_captures.contains(&idx) {
                Some(optimize_val(prog_info, fun_info, capture))
              } else {
                None
              }
            }).collect();
          Some((var, closure_name, filtered))
        } else {
          None
        }
      }).collect()),
    grit::Op::Assign(var_vals) =>
      grit::Op::Assign(var_vals.into_iter().filter_map(|(var, val)| {
        if fun_info.used_vars.contains(&var.0) {
          Some((var, optimize_val(prog_info, fun_info, val)))
        } else {
          None
        }
      }).collect()),
  }
}

fn optimize_jump(prog_info: &ProgInfo, fun_info: &FunInfo,
  jump: grit::Jump) -> grit::Jump 
{
  match jump {
    grit::Jump::Goto(target) =>
      grit::Jump::Goto(target),
    grit::Jump::Return(val) =>
      grit::Jump::Return(optimize_val(prog_info, fun_info, val)),
    grit::Jump::TailCall(callee, args) =>
      grit::Jump::TailCall(optimize_callee(prog_info, fun_info, callee),
        args.map_in_place(|a| optimize_val(prog_info, fun_info, a))),
    grit::Jump::Branch(boolval, then_label, else_label) =>
      grit::Jump::Branch(optimize_boolval(prog_info, fun_info, boolval),
        then_label, else_label),
  }
}

fn optimize_callee(prog_info: &ProgInfo, fun_info: &FunInfo,
  callee: grit::Callee) -> grit::Callee 
{
  match callee {
    grit::Callee::Combinator(callee_name) =>
      grit::Callee::Combinator(callee_name),
    grit::Callee::KnownClosure(callee_name, callee_val) =>
      if prog_info.get(&callee_name).unwrap().used_captures.len() == 0 {
        grit::Callee::Combinator(callee_name)
      } else {
        grit::Callee::KnownClosure(callee_name,
          optimize_val(prog_info, fun_info, callee_val))
      },
    grit::Callee::Unknown(callee_val) =>
      grit::Callee::Unknown(optimize_val(prog_info, fun_info, callee_val)),
  }
}

fn optimize_boolval(prog_info: &ProgInfo, fun_info: &FunInfo,
  boolval: grit::Boolval) -> grit::Boolval 
{
  match boolval {
    grit::Boolval::IsTrue(val) =>
      grit::Boolval::IsTrue(optimize_val(prog_info, fun_info, val)),
    grit::Boolval::IsFalse(val) =>
      grit::Boolval::IsFalse(optimize_val(prog_info, fun_info, val)),
  }
}

fn optimize_val(_prog_info: &ProgInfo, fun_info: &FunInfo,
  val: grit::Val) -> grit::Val 
{
  match val {
    grit::Val::Var(var) => {
      assert!(fun_info.used_vars.contains(&var.0));
      grit::Val::Var(var)
    },
    grit::Val::Arg(idx) =>
      grit::Val::Arg(idx),
    grit::Val::Capture(idx) => {
      assert!(fun_info.used_captures.contains(&idx));
      let (new_idx, _) = fun_info.used_captures.iter().enumerate()
        .find(|&(_, old_idx)| old_idx == idx).unwrap();
      grit::Val::Capture(new_idx)
    },
    grit::Val::Combinator(fun_name) =>
      grit::Val::Combinator(fun_name),
    grit::Val::Obj(obj_name) =>
      grit::Val::Obj(obj_name),
    grit::Val::Int(number) =>
      grit::Val::Int(number),
    grit::Val::True =>
      grit::Val::True,
    grit::Val::False =>
      grit::Val::False,
  }
}

fn collect_prog_info(prog: &grit::ProgDef) -> ProgInfo {
  let mut prog_info: ProgInfo = prog.fun_defs.iter().map(|fun_def| {
    (fun_def.name.clone(), FunInfo {
      used_captures: BitSet::new(),
      used_vars: BitSet::new(),
    })
  }).collect();

  let mut first = true;
  loop {
    let prev_info = prog_info.clone();

    for fun_def in prog.fun_defs.iter() {
      for block in fun_def.blocks.iter() {
        jump_info_update(&mut prog_info, &fun_def.name, &block.jump, first);
        for op in block.ops.iter().rev() {
          op_info_update(&mut prog_info, &fun_def.name, op, first);
        }
      }
    }

    first = false;
    if prev_info == prog_info {
      break
    }
  }

  prog_info
}

fn op_info_update(prog_info: &mut ProgInfo, fun_name: &grit::FunName,
  op: &grit::Op, first: bool) 
{
  match *op {
    grit::Op::Call(_, ref callee, ref args) => {
      if first {
        for arg in args.iter() {
          mark_val_used(prog_info, fun_name, arg);
        }
      }
      mark_callee_used(prog_info, fun_name, callee);
    },
    grit::Op::ExternCall(_, _, ref args) =>
      if first {
        for arg in args.iter() {
          mark_val_used(prog_info, fun_name, arg);
        }
      },
    grit::Op::AllocClos(ref closs) =>
      for &(ref var, ref closure_name, ref captures) in closs.iter() {
        if prog_info.get(fun_name).unwrap().used_vars.contains(&var.0) {
          for idx in prog_info.get(closure_name).unwrap().used_captures.clone().iter() {
            mark_val_used(prog_info, fun_name, &captures[idx]);
          }
        }
      },
    grit::Op::Assign(ref var_vals) =>
      for &(ref var, ref val) in var_vals.iter() {
        if prog_info.get(fun_name).unwrap().used_vars.contains(&var.0) {
          mark_val_used(prog_info, fun_name, val);
        }
      },
  }
}

fn jump_info_update(prog_info: &mut ProgInfo, fun_name: &grit::FunName,
  jump: &grit::Jump, first: bool) 
{
  match *jump {
    grit::Jump::Goto(_) => { },
    grit::Jump::Return(ref val) =>
      if first {
        mark_val_used(prog_info, fun_name, val);
      },
    grit::Jump::TailCall(ref callee, ref args) => {
      if first {
        for arg in args.iter() {
          mark_val_used(prog_info, fun_name, arg);
        }
      }
      mark_callee_used(prog_info, fun_name, callee);
    },
    grit::Jump::Branch(ref boolval, _, _) => 
      if first {
        mark_boolval_used(prog_info, fun_name, boolval);
      },
  }
}

fn mark_val_used(prog_info: &mut ProgInfo, fun_name: &grit::FunName, val: &grit::Val) {
  match *val {
    grit::Val::Var(ref var) => {
      prog_info.get_mut(fun_name).unwrap().used_vars.insert(var.0);
    },
    grit::Val::Capture(idx) => {
      prog_info.get_mut(fun_name).unwrap().used_captures.insert(idx);
    },
    grit::Val::Arg(_) |
    grit::Val::Combinator(_) |
    grit::Val::Obj(_) |
    grit::Val::Int(_) |
    grit::Val::True |
    grit::Val::False => (),
  }
}

fn mark_callee_used(prog_info: &mut ProgInfo, fun_name: &grit::FunName, 
  callee: &grit::Callee) 
{
  match *callee {
    grit::Callee::Combinator(_) => (),
    grit::Callee::KnownClosure(ref callee_name, ref callee_val) =>
      if prog_info.get(callee_name).unwrap().used_captures.len() != 0 {
        mark_val_used(prog_info, fun_name, callee_val);
      },
    grit::Callee::Unknown(ref callee_val) =>
      mark_val_used(prog_info, fun_name, callee_val),
  }
}

fn mark_boolval_used(prog_info: &mut ProgInfo, fun_name: &grit::FunName, 
  boolval: &grit::Boolval) 
{
  match *boolval {
    grit::Boolval::IsTrue(ref val) |
    grit::Boolval::IsFalse(ref val) =>
      mark_val_used(prog_info, fun_name, val),
  }
}
