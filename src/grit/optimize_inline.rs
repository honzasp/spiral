use std::collections::{HashSet, HashMap};
use std::iter::{AdditiveIterator};
use grit;

pub fn optimize(mut prog: grit::ProgDef) -> grit::ProgDef {
  let mut inlinable = HashMap::new();
  loop {
    let mut new_inlinable = false;
    for fun_def in prog.fun_defs.iter() {
      if !inlinable.contains_key(&fun_def.name) && is_inlinable(fun_def) {
        inlinable.insert(fun_def.name.clone(), fun_def.clone());
        new_inlinable = true;
      }
    }

    if new_inlinable {
      prog.fun_defs = prog.fun_defs.map_in_place(|fun_def| {
        fun_inline(&inlinable, fun_def)
      });
    } else {
      return prog
    }
  }
}

struct LabelGen {
  used_labels: HashSet<grit::Label>,
}
impl LabelGen {
  fn gen_label(&mut self, base: &str) -> grit::Label {
    for i in (1..) {
      let label = grit::Label(format!("{}#{}", base, i));
      if self.used_labels.insert(label.clone()) {
        return label
      }
    }
    unreachable!()
  }
}

fn fun_inline(inlinable: &HashMap<grit::FunName, grit::FunDef>, 
  fun_def: grit::FunDef) -> grit::FunDef
{
  let mut label_gen = LabelGen { 
    used_labels: fun_def.blocks.iter().map(|blk| blk.label.clone()).collect(),
  };

  let mut new_fun_def = grit::FunDef {
    blocks: Vec::new(),
    .. fun_def
  };

  for block in fun_def.blocks.into_iter() {
    let mut label = block.label;
    let mut ops = Vec::new();

    for op in block.ops.into_iter() {
      match op {
        grit::Op::Call(var, grit::Callee::Combinator(callee_name), args) =>
          match inlinable.get(&callee_name) {
            Some(callee_def) => {
              let next_label = label_gen.gen_label("inline_return");
              let fun_label = inject_fun_def(&mut new_fun_def, &mut label_gen,
                callee_def.clone(), args, var, next_label.clone());
              new_fun_def.blocks.push(grit::Block {
                label: label,
                ops: ops,
                jump: grit::Jump::Goto(fun_label),
              });

              ops = Vec::new();
              label = next_label;
            },
            None =>
              ops.push(grit::Op::Call(var, grit::Callee::Combinator(callee_name), args))
          },
        other_op => ops.push(other_op),
      }
    }

    new_fun_def.blocks.push(grit::Block {
      label: label,
      ops: ops,
      jump: block.jump,
    })
  }

  new_fun_def
}

struct InjectEnv {
  labels: HashMap<grit::Label, grit::Label>,
  args: Vec<grit::Val>,
  var_shift: usize,
}

fn inject_fun_def(target_fun_def: &mut grit::FunDef, label_gen: &mut LabelGen,
  injected_fun_def: grit::FunDef, args: Vec<grit::Val>,
  return_var: grit::Var, return_label: grit::Label) -> grit::Label
{
  let label_map = injected_fun_def.blocks.iter().map(|block| {
      let new_label = label_gen.gen_label(&block.label.0[..]);
      (block.label.clone(), new_label)
    }).collect();
  let inject_env = InjectEnv {
    labels: label_map,
    args: args,
    var_shift: target_fun_def.var_count,
  };

  for block in injected_fun_def.blocks.into_iter() {
    let mut ops: Vec<_> = block.ops.into_iter().map(|op| {
      match op {
        grit::Op::Call(..) =>
          panic!("fun with call should not have been inlined"),
        grit::Op::ExternCall(ret_var, ext_name, args) =>
          grit::Op::ExternCall(injected_var(&inject_env, ret_var),
            ext_name, args.map_in_place(|a| injected_val(&inject_env, a))),
        grit::Op::AllocClos(closs) =>
          grit::Op::AllocClos(closs.map_in_place(|(var, closure_name, captures)| {
            (injected_var(&inject_env, var), closure_name,
              captures.map_in_place(|c| injected_val(&inject_env, c)))
          })),
        grit::Op::Assign(var_vals) =>
          grit::Op::Assign(var_vals.map_in_place(|(var, val)| {
            (injected_var(&inject_env, var), injected_val(&inject_env, val))
          })),
      }
    }).collect();

    let jump = match block.jump {
      grit::Jump::Goto(target_label) =>
        grit::Jump::Goto(injected_label(&inject_env, target_label)),
      grit::Jump::Return(return_val) => {
        ops.push(grit::Op::Assign(vec![
          (return_var.clone(), injected_val(&inject_env, return_val))]));
        grit::Jump::Goto(return_label.clone())
      },
      grit::Jump::TailCall(..) =>
        panic!("fun with tail call should not have been inlined"),
      grit::Jump::Branch(boolval, then_label, else_label) =>
        grit::Jump::Branch(injected_boolval(&inject_env, boolval),
          injected_label(&inject_env, then_label),
          injected_label(&inject_env, else_label)),
    };

    target_fun_def.blocks.push(grit::Block {
      label: injected_label(&inject_env, block.label),
      ops: ops,
      jump: jump,
    });
  }

  target_fun_def.var_count += injected_fun_def.var_count;
  injected_label(&inject_env, injected_fun_def.start_label)
}

fn injected_var(inject_env: &InjectEnv, var: grit::Var) -> grit::Var {
  grit::Var(inject_env.var_shift + var.0)
}

fn injected_label(inject_env: &InjectEnv, label: grit::Label) -> grit::Label {
  inject_env.labels.get(&label).unwrap().clone()
}

fn injected_val(inject_env: &InjectEnv, val: grit::Val) -> grit::Val {
  match val {
    grit::Val::Var(var) => grit::Val::Var(injected_var(inject_env, var)),
    grit::Val::Arg(idx) => inject_env.args[idx].clone(),
    grit::Val::Capture(_) => panic!("fun with captures should not have been inlined"),
    grit::Val::Combinator(fun_name) => grit::Val::Combinator(fun_name),
    grit::Val::Obj(obj_name) => grit::Val::Obj(obj_name),
    grit::Val::Int(i) => grit::Val::Int(i),
    grit::Val::True => grit::Val::True,
    grit::Val::False => grit::Val::False,
    grit::Val::Undefined => grit::Val::Undefined,
  }
}

fn injected_boolval(inject_env: &InjectEnv, boolval: grit::Boolval) -> grit::Boolval {
  match boolval {
    grit::Boolval::IsTrue(val) =>
      grit::Boolval::IsTrue(injected_val(inject_env, val)),
    grit::Boolval::IsFalse(val) =>
      grit::Boolval::IsFalse(injected_val(inject_env, val)),
  }
}

fn is_inlinable(fun_def: &grit::FunDef) -> bool {
  fun_def.capture_count == 0 && calls_only_extern(fun_def) && fun_size(fun_def) < 50
}

fn calls_only_extern(fun_def: &grit::FunDef) -> bool {
  fun_def.blocks.iter().all(|block| {
    block.ops.iter().all(|op| {
      match *op {
        grit::Op::Call(..) => false,
        grit::Op::ExternCall(..) => true,
        grit::Op::AllocClos(..) => true,
        grit::Op::Assign(..) => true,
      }
    }) && match block.jump {
      grit::Jump::Goto(..) => true,
      grit::Jump::Return(..) => true,
      grit::Jump::TailCall(..) => false,
      grit::Jump::Branch(..) => true,
    }
  })
}

fn fun_size(fun_def: &grit::FunDef) -> usize {
  fun_def.blocks.iter().map(|block| {
    block.ops.iter().map(op_size).sum() + jump_size(&block.jump)
  }).sum()
}

fn op_size(op: &grit::Op) -> usize {
  match *op {
    grit::Op::Call(_, ref callee, ref args) =>
      1 + args.len() + callee_size(callee),
    grit::Op::ExternCall(_, _, ref args) =>
      args.len() + 2,
    grit::Op::AllocClos(ref closs) =>
      closs.iter().map(|&(_, _, ref captures)| captures.len() + 2).sum(),
    grit::Op::Assign(ref var_vals) =>
      var_vals.len(),
  }
}

fn jump_size(jump: &grit::Jump) -> usize {
  match *jump {
    grit::Jump::Goto(_) => 0,
    grit::Jump::Return(_) => 1,
    grit::Jump::TailCall(ref callee, ref args) =>
      args.len() + callee_size(callee),
    grit::Jump::Branch(_, _, _) => 1,
  }
}

fn callee_size(callee: &grit::Callee) -> usize {
  match *callee {
    grit::Callee::Combinator(..) => 0,
    grit::Callee::KnownClosure(..) => 1,
    grit::Callee::Unknown(..) => 3,
  }
}
