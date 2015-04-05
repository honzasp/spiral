use std::collections::{HashMap};
use grit;

pub fn optimize(prog: grit::ProgDef) -> grit::ProgDef {
  let mut prog = prog;
  let prog_info = collect_prog_info(&prog);
  prog.fun_defs = prog.fun_defs.map_in_place(|mut fun_def| {
    let fun_info = prog_info.get(&fun_def.name).unwrap();
    fun_def.blocks = fun_def.blocks.map_in_place(|mut block| {
      block.ops = block.ops.map_in_place(|op| update_op_callees(fun_info, op));
      block.jump = update_jump_callees(fun_info, block.jump);
      block
    });
    fun_def
  });
  prog
}

type ProgInfo = HashMap<grit::FunName, FunInfo>;

#[derive(Debug, PartialEq, Clone)]
struct FunInfo {
  capture_infos: Vec<Info>,
  var_infos: Vec<Info>,
  return_info: Info,
}

#[derive(Debug, PartialEq, Clone)]
enum Info {
  Any,
  Closure(grit::FunName),
  Combinator(grit::FunName),
  No,
}

fn update_op_callees(fun_info: &FunInfo, op: grit::Op) -> grit::Op {
  match op {
    grit::Op::Call(target_var, callee, args) =>
      grit::Op::Call(target_var, update_callee(fun_info, callee), args),
    op => op,
  }
}


fn update_jump_callees(fun_info: &FunInfo, jump: grit::Jump) -> grit::Jump {
  match jump {
    grit::Jump::TailCall(callee, args) =>
      grit::Jump::TailCall(update_callee(fun_info, callee), args),
    jump => jump,
  }
}

fn update_callee(fun_info: &FunInfo, callee: grit::Callee) -> grit::Callee {
  match callee {
    grit::Callee::Unknown(val) => 
      match val_info(fun_info, &val) {
        Info::Closure(fun_name) =>
          grit::Callee::KnownClosure(fun_name, val),
        Info::Combinator(fun_name) =>
          grit::Callee::Combinator(fun_name),
        Info::Any | Info::No =>
          grit::Callee::Unknown(val)
      },
    callee => callee,
  }
}

fn collect_prog_info(prog: &grit::ProgDef) -> ProgInfo {
  let mut prog_info: ProgInfo = prog.fun_defs.iter().map(|fun_def| {
    (fun_def.name.clone(), FunInfo {
      capture_infos: (0..fun_def.capture_count).map(|_| Info::Any).collect(),
      var_infos: (0..fun_def.var_count).map(|_| Info::Any).collect(),
      return_info: Info::Any,
    })
  }).collect();

  loop {
    let prev_info = prog_info.clone();

    for fun_def in prog.fun_defs.iter() {
      for block in fun_def.blocks.iter() {
        for op in block.ops.iter() {
          op_info_update(&mut prog_info, &fun_def.name, op);
        }
        jump_info_update(&mut prog_info, &fun_def.name, &block.jump);
      }
    }

    if prev_info == prog_info {
      break
    }
  }

  prog_info
}

fn op_info_update(prog_info: &mut ProgInfo, fun_name: &grit::FunName, op: &grit::Op) {
  match *op {
    grit::Op::Call(ref result_var, ref callee, _) => 
      prog_info.get_mut(fun_name).unwrap().var_infos[result_var.0] = join_info(
        &prog_info.get(fun_name).unwrap().var_infos[result_var.0],
        callee_return_info(prog_info, callee)),
    grit::Op::ExternCall(ref result_var, _, _) => 
      prog_info.get_mut(fun_name).unwrap().var_infos[result_var.0] = Info::No,
    grit::Op::AllocClos(ref closs) =>
      for &(ref clos_var, ref clos_name, ref captures) in closs.iter() {
        prog_info.get_mut(fun_name).unwrap().var_infos[clos_var.0] = join_info(
          &prog_info.get(fun_name).unwrap().var_infos[clos_var.0],
          Info::Closure(clos_name.clone()));
        for (idx, capture_val) in captures.iter().enumerate() {
          prog_info.get_mut(clos_name).unwrap().capture_infos[idx] = join_info(
            &prog_info.get(clos_name).unwrap().capture_infos[idx],
            val_info(prog_info.get(fun_name).unwrap(), capture_val));
        }
      },
    grit::Op::Assign(ref var_vals) =>
      for &(ref var, ref val) in var_vals.iter() {
        prog_info.get_mut(fun_name).unwrap().var_infos[var.0] = join_info(
          &prog_info.get(fun_name).unwrap().var_infos[var.0],
          val_info(prog_info.get(fun_name).unwrap(), val));
      },
  }
}

fn jump_info_update(prog_info: &mut ProgInfo,
  fun_name: &grit::FunName, jump: &grit::Jump) 
{
  match *jump {
    grit::Jump::Goto(_) => { },
    grit::Jump::Return(ref val) =>
      prog_info.get_mut(fun_name).unwrap().return_info = join_info(
        &prog_info.get(fun_name).unwrap().return_info,
        val_info(prog_info.get(fun_name).unwrap(), val)),
    grit::Jump::TailCall(ref callee, _) =>
      prog_info.get_mut(fun_name).unwrap().return_info = join_info(
        &prog_info.get(fun_name).unwrap().return_info,
        callee_return_info(prog_info, callee)),
    grit::Jump::Branch(_, _, _) => { },
  }
}

fn callee_return_info(prog_info: &ProgInfo, callee: &grit::Callee) -> Info {
  match *callee {
    grit::Callee::Combinator(ref callee_name) |
    grit::Callee::KnownClosure(ref callee_name, _) => 
      prog_info.get(callee_name).unwrap().return_info.clone(),
    grit::Callee::Unknown(_) =>
      Info::No,
  }
}

fn val_info(fun_info: &FunInfo, val: &grit::Val) -> Info {
  match *val {
    grit::Val::Var(ref var) => fun_info.var_infos[var.0].clone(),
    grit::Val::Arg(_) => Info::No,
    grit::Val::Capture(idx) => fun_info.capture_infos[idx].clone(),
    grit::Val::Combinator(ref fun_name) => Info::Combinator(fun_name.clone()),
    grit::Val::Obj(_) | grit::Val::Int(_) => Info::No,
    grit::Val::True | grit::Val::False => Info::No,
  }
}

fn join_info(info_1: &Info, info_2: Info) -> Info {
  match *info_1 {
    Info::Any => info_2,
    Info::Closure(ref name_1) => match info_2 {
      Info::Closure(name_2) => 
        if *name_1 == name_2 {
          Info::Closure(name_2)
        } else {
          Info::No
        },
      Info::Any =>
        Info::Closure(name_1.clone()),
      _ => 
        Info::No,
    },
    Info::Combinator(ref name_1) => match info_2 {
      Info::Combinator(name_2) => 
        if *name_1 == name_2 {
          Info::Combinator(name_2)
        } else {
          Info::No
        },
      Info::Any =>
        Info::Combinator(name_1.clone()),
      _ => 
        Info::No,
    },
    Info::No => Info::No,
  }
}