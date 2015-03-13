use std::collections::{HashSet};
use spine;
use spine::census;
use grit;

type Env = spine::env::Env<grit::Slot, (), ContBind>;

#[derive(Debug)]
enum ContBind {
  Cont(Vec<grit::Slot>, grit::Label),
  Return,
}

#[derive(Debug)]
struct FunSt {
  slot_count: usize,
  blocks: Vec<grit::Block>,
  labels: HashSet<grit::Label>,
}

impl FunSt {
  fn gen_label(&mut self, base: &str) -> grit::Label {
    for i in 1.. {
      let label = grit::Label(format!("{}_{}", base, i));
      if self.labels.insert(label.clone()) {
        return label
      }
    }
    unreachable!()
  }

  fn use_slot(&mut self, slot: &grit::Slot) {
    if slot.0 + 1 > self.slot_count {
      self.slot_count = slot.0 + 1;
    }
  }
}

pub fn grit_from_spine(prog: &spine::ProgDef) -> grit::ProgDef {
  grit::ProgDef {
    fun_defs: prog.fun_defs.iter().map(translate_fun_def).collect(),
    main_fun: grit::FunName(prog.main_fun.0.clone()),
  }
}

fn translate_fun_def(fun_def: &spine::FunDef) -> grit::FunDef {
  let empty_env = spine::env::Env::new();
  let ret_env = empty_env.bind_conts(vec![(fun_def.ret.clone(), ContBind::Return)]);
  let args_env = ret_env.bind_vars(fun_def.args.iter().enumerate()
      .map(|(slot, arg)| (arg.clone(), grit::Slot(slot))).collect());

  let mut st = FunSt {
    slot_count: fun_def.args.len(),
    blocks: Vec::new(),
    labels: HashSet::new(),
  };
  let start_label = st.gen_label("start");

  let (census_body, _) = census::census_from_term(&fun_def.body);
  translate_term(&mut st, &args_env, start_label.clone(), &census_body);

  grit::FunDef {
    name: grit::FunName(fun_def.name.0.clone()),
    arg_count: fun_def.args.len(),
    slot_count: st.slot_count,
    blocks: st.blocks,
    start_label: start_label,
  }
}

fn translate_term(st: &mut FunSt, env: &Env, label: grit::Label, term: &census::Term) {
  match *term {
    census::Term::Letcont(ref cont_defs, ref body) => 
      translate_letcont(st, env, label, &cont_defs[..], &**body),
    census::Term::Call(ref fun_name, ref cont_name, ref args) => 
      translate_call(st, env, label, fun_name, cont_name, &args[..]),
    census::Term::ExternCall(ref extern_name, ref cont_name, ref args) => 
      translate_extern_call(st, env, label, extern_name, cont_name, &args[..]),
    census::Term::Cont(ref cont, ref args) => 
      translate_cont(st, env, label, cont, &args[..]),
    census::Term::Branch(ref boolval, ref then_cont, ref else_cont) => 
      translate_branch(st, env, label, boolval, then_cont, else_cont),
  }
}

fn translate_letcont(st: &mut FunSt, env: &Env, label: grit::Label, 
  cont_defs: &[census::ContDef], body: &census::Term)
{
  let labels: Vec<_> = cont_defs.iter()
    .map(|cont_def| st.gen_label(&cont_def.name.0[..])).collect();

  let arg_slotss: Vec<Vec<_>> = cont_defs.iter().map(|cont_def| {
      let used_slots: HashSet<_> = cont_def.free_vars.iter()
        .map(|free_var| env.lookup_var(free_var).expect("undefined var"))
        .collect();
      (0..).map(grit::Slot).filter(|slot| !used_slots.contains(slot))
        .take(cont_def.args.len()).collect()
    }).collect();

  let cont_binds = cont_defs.iter().zip(labels.iter()).zip(arg_slotss.iter())
    .map(|((cont_def, label), arg_slots)| {
      (cont_def.name.clone(), ContBind::Cont(arg_slots.clone(), label.clone()))
    }).collect();
  let outer_env = env.bind_conts(cont_binds);

  for ((cont_def, label), arg_slots) in cont_defs.iter()
    .zip(labels.into_iter()).zip(arg_slotss.into_iter())
  {
    for arg_slot in arg_slots.iter() {
      st.use_slot(arg_slot);
    }

    let inner_env = outer_env.bind_vars(
        cont_def.args.iter()
          .zip(arg_slots.into_iter())
          .map(|(arg, arg_slot)| (arg.clone(), arg_slot)).collect());
    translate_term(st, &inner_env, label.clone(), &cont_def.body);
  }

  translate_term(st, &outer_env, label, body)
}

fn translate_call(st: &mut FunSt, env: &Env, label: grit::Label,
  fun_name: &spine::FunName, ret_cont: &spine::ContName, args: &[spine::Val])
{
  let cont_bind = env.lookup_cont(ret_cont).expect("undefined return cont");
  let grit_name = grit::FunName(fun_name.0.clone());
  let grit_args = args.iter().map(|arg| translate_val(st, env, arg)).collect();

  st.blocks.push(match *cont_bind {
    ContBind::Cont(ref arg_slots, ref target_label) => {
      assert_eq!(arg_slots.len(), 1);
      grit::Block {
        label: label,
        ops: vec![grit::Op::Call(arg_slots[0].clone(), grit_name, grit_args)],
        jump: grit::Jump::Goto(target_label.clone()),
      }
    },
    ContBind::Return => 
      grit::Block {
        label: label,
        ops: vec![],
        jump: grit::Jump::TailCall(grit_name, grit_args),
      },
  });
}

fn translate_extern_call(st: &mut FunSt, env: &Env, label: grit::Label,
  extern_name: &spine::ExternName, ret_cont: &spine::ContName, args: &[spine::Val])
{
  let cont_bind = env.lookup_cont(ret_cont).expect("undefined return cont");
  let grit_name = grit::ExternName(extern_name.0.clone());
  let grit_args = args.iter().map(|arg| translate_val(st, env, arg)).collect();

  match *cont_bind {
    ContBind::Cont(ref arg_slots, ref target_label) => {
      assert_eq!(arg_slots.len(), 1);
      st.blocks.push(grit::Block {
        label: label,
        ops: vec![grit::Op::ExternCall(arg_slots[0].clone(), grit_name, grit_args)],
        jump: grit::Jump::Goto(target_label.clone()),
      })
    },
    ContBind::Return => {
      let slot = grit::Slot(0);
      st.use_slot(&slot);
      st.blocks.push(grit::Block {
        label: label,
        ops: vec![grit::Op::ExternCall(slot.clone(), grit_name, grit_args)],
        jump: grit::Jump::Return(grit::Val::Slot(slot)),
      })
    },
  }
}

fn translate_cont(st: &mut FunSt, env: &Env, label: grit::Label,
  cont: &spine::ContName, args: &[spine::Val])
{
  let cont_bind = env.lookup_cont(cont).expect("undefined cont");
  let grit_args: Vec<_> = args.iter().map(|arg| translate_val(st, env, arg)).collect();

  match *cont_bind {
    ContBind::Cont(ref arg_slots, ref target_label) => {
      assert_eq!(arg_slots.len(), args.len());
      st.blocks.push(grit::Block {
        label: label,
        ops: vec![grit::Op::Assign(
          arg_slots.iter().cloned().zip(grit_args.into_iter()).collect())],
        jump: grit::Jump::Goto(target_label.clone()),
      })
    },
    ContBind::Return => {
      assert_eq!(args.len(), 1);
      st.blocks.push(grit::Block {
        label: label,
        ops: vec![],
        jump: grit::Jump::Return(grit_args[0].clone()),
      })
    },
  }
}

fn translate_branch(st: &mut FunSt, env: &Env, label: grit::Label,
  boolval: &spine::Boolval, then_cont: &spine::ContName, else_cont: &spine::ContName)
{
  let then_bind = env.lookup_cont(then_cont).expect("undefined then cont");
  let then_label = match *then_bind {
    ContBind::Cont(ref slots, ref label) => {
      assert_eq!(slots.len(), 0);
      label.clone()
    },
    ContBind::Return => panic!("return cont in branch"),
  };

  let else_bind = env.lookup_cont(else_cont).expect("undefined else cont");
  let else_label = match *else_bind {
    ContBind::Cont(ref slots, ref label) => {
      assert_eq!(slots.len(), 0);
      label.clone()
    },
    ContBind::Return => panic!("return cont in branch"),
  };

  let boolval = translate_boolval(st, env, boolval);
  st.blocks.push(grit::Block {
      label: label,
      ops: vec![],
      jump: grit::Jump::Branch(boolval, then_label, else_label),
    });
}

fn translate_val(_: &mut FunSt, env: &Env, val: &spine::Val) -> grit::Val {
  match *val {
    spine::Val::Int(num) =>
      grit::Val::Int(num),
    spine::Val::Var(ref var) =>
      grit::Val::Slot(env.lookup_var(var).expect("undefined var").clone()),
    spine::Val::True => grit::Val::True,
    spine::Val::False => grit::Val::False,
  }
}

fn translate_boolval(st: &mut FunSt, env: &Env, boolval: &spine::Boolval) 
  -> grit::Boolval 
{
  match *boolval {
    spine::Boolval::IsTrue(ref val) =>
      grit::Boolval::IsTrue(translate_val(st, env, val)),
    spine::Boolval::IsFalse(ref val) =>
      grit::Boolval::IsFalse(translate_val(st, env, val)),
  }
}
