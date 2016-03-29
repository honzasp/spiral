use std::collections::{HashMap};
use grit;
use map_in_place::{map_in_place};

pub fn optimize(prog: grit::ProgDef) -> grit::ProgDef {
  let mut prog = prog;
  let prog_info = collect_prog_info(&prog);
  prog.fun_defs = map_in_place(prog.fun_defs, |mut fun_def| {
    let fun_info = prog_info.get(&fun_def.name).unwrap();
    fun_def.blocks = map_in_place(fun_def.blocks, |mut block| {
      block.ops = map_in_place(block.ops, |op| 
        update_op(&prog_info, fun_info, op));
      block.jump = update_jump(&prog_info, fun_info, block.jump);
      block
    });
    fun_def
  });
  prog
}

type ProgInfo = HashMap<grit::FunName, FunInfo>;

#[derive(Debug, PartialEq, Clone)]
struct FunInfo {
  arg_count: usize,
  capture_infos: Vec<Info>,
  var_infos: Vec<Info>,
  return_info: Info,
}

#[derive(Debug, PartialEq, Clone)]
enum Info {
  Any,
  Fun(grit::FunName),
  False,
  True,
  Int(i32),
  Obj(grit::ObjName),
  AnyTrue,
  No,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum BoolInfo {
  Any,
  True,
  False,
  No,
}

fn update_op(prog_info: &ProgInfo, fun_info: &FunInfo, op: grit::Op) -> grit::Op {
  match op {
    grit::Op::Call(target_var, callee, args) =>
      grit::Op::Call(target_var,
        update_callee(prog_info, fun_info, callee, args.len()),
        map_in_place(args, |a| update_val(prog_info, fun_info, a))),
    grit::Op::ExternCall(target_var, ext_name, args) =>
      grit::Op::ExternCall(target_var, ext_name,
        map_in_place(args, |a| update_val(prog_info, fun_info, a))),
    grit::Op::AllocClos(closs) =>
      grit::Op::AllocClos(map_in_place(closs, |(var, fun_name, captures)| {
        (var, fun_name, map_in_place(captures, |c| update_val(prog_info, fun_info, c)))
      })),
    grit::Op::Assign(var_vals) =>
      grit::Op::Assign(map_in_place(var_vals, |(var, val)| {
        (var, update_val(prog_info, fun_info, val))
      })),
  }
}


fn update_jump(prog_info: &ProgInfo, fun_info: &FunInfo, jump: grit::Jump) -> grit::Jump {
  match jump {
    grit::Jump::Goto(label) =>
      grit::Jump::Goto(label),
    grit::Jump::Return(val) =>
      grit::Jump::Return(update_val(prog_info, fun_info, val)),
    grit::Jump::TailCall(callee, args) =>
      grit::Jump::TailCall(update_callee(prog_info, fun_info, callee, args.len()),
        map_in_place(args, |arg| update_val(prog_info, fun_info, arg))),
    grit::Jump::Branch(boolval, then_label, else_label) =>
      match boolval_info(fun_info, &boolval) {
        BoolInfo::True => grit::Jump::Goto(then_label),
        BoolInfo::False => grit::Jump::Goto(else_label),
        BoolInfo::Any => 
          grit::Jump::Branch(grit::Boolval::IsTrue(grit::Val::Undefined),
            then_label, else_label),
        BoolInfo::No =>
          grit::Jump::Branch(update_boolval(prog_info, fun_info, boolval),
            then_label, else_label),
      },
  }
}

fn update_callee(prog_info: &ProgInfo, fun_info: &FunInfo,
  callee: grit::Callee, arg_count: usize) -> grit::Callee 
{
  match callee {
    grit::Callee::Unknown(val) => 
      match val_info(fun_info, &val) {
        Info::Fun(callee_name) => {
          let callee_info = prog_info.get(&callee_name).unwrap();
          if callee_info.arg_count == arg_count {
            if callee_info.capture_infos.len() == 0 {
              grit::Callee::Combinator(callee_name)
            } else {
              grit::Callee::KnownClosure(callee_name, val)
            }
          } else {
            grit::Callee::Unknown(val)
          }
        },
        _ =>
          grit::Callee::Unknown(val)
      },
    callee => callee,
  }
}

fn update_val(prog_info: &ProgInfo, fun_info: &FunInfo, val: grit::Val) -> grit::Val {
  match val_info(fun_info, &val) {
    Info::Any => grit::Val::Undefined,
    Info::Fun(fun_name) => 
      if prog_info.get(&fun_name).unwrap().capture_infos.len() == 0 {
        grit::Val::Combinator(fun_name)
      } else {
        val
      },
    Info::False => grit::Val::False,
    Info::True => grit::Val::True,
    Info::Int(num) => grit::Val::Int(num),
    Info::Obj(name) => grit::Val::Obj(name),
    Info::AnyTrue | Info::No => val,
  }
}

fn update_boolval(prog_info: &ProgInfo, fun_info: &FunInfo,
  boolval: grit::Boolval) -> grit::Boolval 
{
  match boolval {
    grit::Boolval::IsTrue(val) =>
      grit::Boolval::IsTrue(update_val(prog_info, fun_info, val)),
    grit::Boolval::IsFalse(val) =>
      grit::Boolval::IsFalse(update_val(prog_info, fun_info, val)),
  }
}

fn collect_prog_info(prog: &grit::ProgDef) -> ProgInfo {
  let mut prog_info: ProgInfo = prog.fun_defs.iter().map(|fun_def| {
    (fun_def.name.clone(), FunInfo {
      arg_count: fun_def.arg_count,
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
        &return_info(prog_info, callee)),
    grit::Op::ExternCall(ref result_var, _, _) => 
      prog_info.get_mut(fun_name).unwrap().var_infos[result_var.0] = Info::No,
    grit::Op::AllocClos(ref closs) =>
      for &(ref clos_var, ref clos_name, ref captures) in closs.iter() {
        prog_info.get_mut(fun_name).unwrap().var_infos[clos_var.0] = join_info(
          &prog_info.get(fun_name).unwrap().var_infos[clos_var.0],
          &Info::Fun(clos_name.clone()));
        for (idx, capture_val) in captures.iter().enumerate() {
          prog_info.get_mut(clos_name).unwrap().capture_infos[idx] = join_info(
            &prog_info.get(clos_name).unwrap().capture_infos[idx],
            &val_info(prog_info.get(fun_name).unwrap(), capture_val));
        }
      },
    grit::Op::Assign(ref var_vals) =>
      for &(ref var, ref val) in var_vals.iter() {
        prog_info.get_mut(fun_name).unwrap().var_infos[var.0] = join_info(
          &prog_info.get(fun_name).unwrap().var_infos[var.0],
          &val_info(prog_info.get(fun_name).unwrap(), val));
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
        &val_info(prog_info.get(fun_name).unwrap(), val)),
    grit::Jump::TailCall(ref callee, _) =>
      prog_info.get_mut(fun_name).unwrap().return_info = join_info(
        &prog_info.get(fun_name).unwrap().return_info,
        &return_info(prog_info, callee)),
    grit::Jump::Branch(_, _, _) => { },
  }
}

fn return_info(prog_info: &ProgInfo, callee: &grit::Callee) -> Info {
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
    grit::Val::Combinator(ref fun_name) => Info::Fun(fun_name.clone()),
    grit::Val::Obj(ref obj_name) => Info::Obj(obj_name.clone()),
    grit::Val::Int(num) => Info::Int(num),
    grit::Val::True => Info::True,
    grit::Val::False => Info::False,
    grit::Val::Undefined => Info::Any,
  }
}

fn boolval_info(fun_info: &FunInfo, boolval: &grit::Boolval) -> BoolInfo {
  match *boolval {
    grit::Boolval::IsTrue(ref val) =>
      info_to_bool(&val_info(fun_info, val)),
    grit::Boolval::IsFalse(ref val) =>
      negate_boolinfo(info_to_bool(&val_info(fun_info, val))),
  }
}

fn negate_boolinfo(boolinfo: BoolInfo) -> BoolInfo {
  match boolinfo {
    BoolInfo::Any => BoolInfo::Any,
    BoolInfo::True => BoolInfo::False,
    BoolInfo::False => BoolInfo::True,
    BoolInfo::No => BoolInfo::No,
  }
}

fn info_to_bool(info: &Info) -> BoolInfo {
  match *info {
    Info::Any => BoolInfo::Any,
    Info::No => BoolInfo::No,
    Info::Fun(_) | Info::Int(_) | Info::Obj(_) => BoolInfo::True,
    Info::True | Info::AnyTrue => BoolInfo::True,
    Info::False => BoolInfo::False,
  }
}

fn join_info(info_1: &Info, info_2: &Info) -> Info {
  match (info_1, info_2) {
    (&Info::Any, info_2) => info_2.clone(),
    (info_1, &Info::Any) => info_1.clone(),
    (&Info::No, _) => Info::No,
    (_, &Info::No) => Info::No,
    (&Info::Fun(ref name_1), &Info::Fun(ref name_2)) =>
      if name_1 == name_2 {
        Info::Fun(name_1.clone())
      } else {
        Info::AnyTrue
      },
    (&Info::Int(num_1), &Info::Int(num_2)) =>
      if num_1 == num_2 {
        Info::Int(num_1)
      } else {
        Info::AnyTrue
      },
    (&Info::Obj(ref name_1), &Info::Obj(ref name_2)) =>
      if name_1 == name_2 {
        Info::Obj(name_1.clone())
      } else {
        Info::AnyTrue
      },
    (&Info::False, &Info::False) => Info::False,
    (&Info::True, &Info::True) => Info::True,
    (&Info::False, _) => Info::No,
    (_, &Info::False) => Info::No,
    (_, _) => Info::AnyTrue,
  }
}
