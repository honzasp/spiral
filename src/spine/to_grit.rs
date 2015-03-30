use std::collections::{HashSet};
use spine;
use grit;

type Env = spine::env::Env<VarBind, usize, ContBind>;

#[derive(Debug)]
#[allow(dead_code)]
enum VarBind {
  Val(grit::Val),
  Combinator(spine::FunName),
  KnownClosure(spine::FunName, grit::Val),
}

#[derive(Debug)]
enum ContBind {
  Cont(Vec<grit::Var>, grit::Label),
  Return,
}

#[derive(Debug)]
struct FunSt {
  var_count: usize,
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

  fn gen_var(&mut self) -> grit::Var {
    self.var_count += 1;
    grit::Var(self.var_count - 1)
  }
}

pub fn grit_from_spine(prog: &spine::ProgDef) -> grit::ProgDef {
  let fun_binds = prog.fun_defs.iter().map(|fun_def| {
      (fun_def.name.clone(), fun_def.args.len())
    }).collect();
  let env = spine::env::Env::new().bind_funs(fun_binds);

  grit::ProgDef {
    fun_defs: prog.fun_defs.iter()
      .map(|fun_def| translate_fun_def(&env, fun_def)).collect(),
    main_fun: translate_fun_name(&prog.main_fun),
  }
}

fn translate_fun_def(env: &Env, fun_def: &spine::FunDef) -> grit::FunDef {
  let ret_bind = (fun_def.ret.clone(), ContBind::Return);
  let arg_binds = fun_def.args.iter().enumerate()
      .map(|(idx, arg)| (arg.clone(), VarBind::Val(grit::Val::Arg(idx))));
  let capture_binds = fun_def.captures.iter().enumerate()
      .map(|(idx, capture)| (capture.clone(), VarBind::Val(grit::Val::Capture(idx))));

  let env = env
    .bind_conts(vec![ret_bind])
    .bind_vars(arg_binds.chain(capture_binds).collect());

  let mut st = FunSt {
    var_count: 0,
    blocks: Vec::new(),
    labels: HashSet::new(),
  };
  let start_label = st.gen_label("start");

  translate_term(&mut st, &env, start_label.clone(), &fun_def.body);

  grit::FunDef {
    name: translate_fun_name(&fun_def.name),
    capture_count: fun_def.captures.len(),
    arg_count: fun_def.args.len(),
    var_count: st.var_count,
    blocks: st.blocks,
    start_label: start_label,
  }
}

fn translate_term(st: &mut FunSt, env: &Env, label: grit::Label, term: &spine::Term) {
  match *term {
    spine::Term::Letcont(ref cont_defs, ref body) => 
      translate_letcont(st, env, label, &cont_defs[..], &**body),
    spine::Term::Letclos(ref clos_defs, ref body) => 
      translate_letclos(st, env, label, &clos_defs[..], &**body),
    spine::Term::Call(ref fun, ref cont_name, ref args) => 
      translate_call(st, env, label, fun, cont_name, &args[..]),
    spine::Term::ExternCall(ref extern_name, ref cont_name, ref args) => 
      translate_extern_call(st, env, label, extern_name, cont_name, &args[..]),
    spine::Term::Cont(ref cont, ref args) => 
      translate_cont(st, env, label, cont, &args[..]),
    spine::Term::Branch(ref boolval, ref then_cont, ref else_cont) => 
      translate_branch(st, env, label, boolval, then_cont, else_cont),
  }
}

fn translate_letcont(st: &mut FunSt, env: &Env, label: grit::Label,
  cont_defs: &[spine::ContDef], body: &spine::Term)
{
  let labels: Vec<_> = cont_defs.iter()
    .map(|cont_def| st.gen_label(&cont_def.name.0[..])).collect();

  let arg_varss: Vec<Vec<_>> = cont_defs.iter().map(|cont_def| {
      cont_def.args.iter().map(|_| st.gen_var()).collect()
    }).collect();

  let cont_binds = cont_defs.iter().zip(labels.iter()).zip(arg_varss.iter())
    .map(|((cont_def, label), arg_vars)| {
      let bind = ContBind::Cont(arg_vars.clone(), label.clone());
      (cont_def.name.clone(), bind)
    }).collect();
  let outer_env = env.bind_conts(cont_binds);

  for ((cont_def, label), arg_vars) in cont_defs.iter()
    .zip(labels.into_iter()).zip(arg_varss.into_iter())
  {
    let inner_env = outer_env.bind_vars(
      cont_def.args.iter()
        .zip(arg_vars.into_iter())
        .map(|(arg, arg_var)| {
          (arg.clone(), VarBind::Val(grit::Val::Var(arg_var)))
        }).collect());
    translate_term(st, &inner_env, label.clone(), &cont_def.body);
  }

  translate_term(st, &outer_env, label, body)
}

fn translate_letclos(st: &mut FunSt, env: &Env, 
  label: grit::Label, clos_defs: &[spine::ClosureDef], body: &spine::Term)
{
  let body_label = st.gen_label("letclos_body");
  let clos_vars: Vec<_> = clos_defs.iter().map(|_| st.gen_var()).collect();

  let clos_binds = clos_defs.iter().zip(clos_vars.iter())
    .map(|(clos_def, var)| 
        (clos_def.var.clone(), VarBind::Val(grit::Val::Var(var.clone()))))
    .collect();
  let inner_env = env.bind_vars(clos_binds);

  let alloc_clos = clos_vars.into_iter()
    .zip(clos_defs.iter())
    .map(|(clos_var, clos_def)| {
      let captures = clos_def.captures.iter().map(|capt| 
          translate_val(st, &inner_env, capt)).collect();
      (clos_var, translate_fun_name(&clos_def.fun_name), captures)
    }).collect();

  st.blocks.push(grit::Block {
    label: label.clone(),
    ops: vec![grit::Op::AllocClos(alloc_clos)],
    jump: grit::Jump::Goto(body_label.clone()),
  });
         
  translate_term(st, &inner_env, body_label, body);
}

fn translate_call(st: &mut FunSt, env: &Env, label: grit::Label,
  fun: &spine::Val, ret_cont: &spine::ContName, args: &[spine::Val])
{
  let cont_bind = env.lookup_cont(ret_cont).expect("undefined return cont");
  let grit_callee = translate_callee(st, env, fun, args.len());
  let grit_args = args.iter().map(|arg| translate_val(st, env, arg)).collect();

  st.blocks.push(match *cont_bind {
    ContBind::Cont(ref arg_vars, ref target_label) => {
      assert_eq!(arg_vars.len(), 1);
      grit::Block {
        label: label,
        ops: vec![grit::Op::Call(arg_vars[0].clone(), grit_callee, grit_args)],
        jump: grit::Jump::Goto(target_label.clone()),
      }
    },
    ContBind::Return => 
      grit::Block {
        label: label,
        ops: vec![],
        jump: grit::Jump::TailCall(grit_callee, grit_args),
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
    ContBind::Cont(ref arg_vars, ref target_label) => {
      assert_eq!(arg_vars.len(), 1);
      st.blocks.push(grit::Block {
        label: label,
        ops: vec![grit::Op::ExternCall(arg_vars[0].clone(), grit_name, grit_args)],
        jump: grit::Jump::Goto(target_label.clone()),
      })
    },
    ContBind::Return => {
      let tmp_var = st.gen_var();
      st.blocks.push(grit::Block {
        label: label,
        ops: vec![grit::Op::ExternCall(tmp_var.clone(), grit_name, grit_args)],
        jump: grit::Jump::Return(grit::Val::Var(tmp_var)),
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
    ContBind::Cont(ref arg_vars, ref target_label) => {
      assert_eq!(arg_vars.len(), args.len());
      st.blocks.push(grit::Block {
        label: label,
        ops: vec![grit::Op::Assign(
          arg_vars.iter().cloned().zip(grit_args.into_iter()).collect())],
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
    ContBind::Cont(ref vars, ref label) => {
      assert_eq!(vars.len(), 0);
      label.clone()
    },
    ContBind::Return => panic!("return cont in branch"),
  };

  let else_bind = env.lookup_cont(else_cont).expect("undefined else cont");
  let else_label = match *else_bind {
    ContBind::Cont(ref vars, ref label) => {
      assert_eq!(vars.len(), 0);
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
    spine::Val::Combinator(ref fun_name) =>
      grit::Val::Combinator(translate_fun_name(fun_name)),
    spine::Val::Int(num) =>
      grit::Val::Int(num),
    spine::Val::Var(ref var) => match *env.lookup_var(var).unwrap() {
      VarBind::Val(ref val) => val.clone(),
      VarBind::Combinator(ref fun_name) =>
        grit::Val::Combinator(translate_fun_name(&fun_name.clone())),
      VarBind::KnownClosure(_, ref val) => val.clone(),
    },
    spine::Val::True => grit::Val::True,
    spine::Val::False => grit::Val::False,
  }
}

fn translate_callee(st: &mut FunSt, env: &Env, val: &spine::Val, argc: usize) 
  -> grit::Callee 
{
  match *val {
    spine::Val::Combinator(ref fun_name) =>
      if *env.lookup_fun(fun_name).expect("undefined fun") == argc {
        return grit::Callee::Combinator(translate_fun_name(fun_name))
      },
    spine::Val::Var(ref var) => match *env.lookup_var(var).unwrap() {
      VarBind::Combinator(ref fun_name) =>
        if *env.lookup_fun(fun_name).expect("undefined fun") == argc {
          return grit::Callee::Combinator(translate_fun_name(fun_name))
        },
      VarBind::KnownClosure(ref fun_name, ref val) => 
        if *env.lookup_fun(fun_name).expect("undefined fun") == argc {
          return grit::Callee::KnownClosure(translate_fun_name(fun_name), val.clone())
        },
      _ => ()
    },
    _ => ()
  }

  grit::Callee::Unknown(translate_val(st, env, val))
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

fn translate_fun_name(name: &spine::FunName) -> grit::FunName {
  grit::FunName(name.0.clone())
}
