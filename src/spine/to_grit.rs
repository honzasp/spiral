use std::collections::{HashSet};
use spine;
use grit;

type Env = spine::env::Env<grit::Val, ContBind>;

#[derive(Debug)]
enum ContBind {
  Cont(Vec<grit::Var>, grit::Label),
  Return,
}

#[derive(Debug)]
struct ProgSt {
  fun_names: HashSet<grit::FunName>,
  obj_names: HashSet<grit::ObjName>,
  fun_defs: Vec<grit::FunDef>,
  obj_defs: Vec<grit::ObjDef>,
}

#[derive(Debug)]
struct FunSt<'p> {
  prog_st: &'p mut ProgSt,
  var_count: usize,
  blocks: Vec<grit::Block>,
  labels: HashSet<grit::Label>,
}

impl ProgSt {
  fn gen_fun_name(&mut self, base: &str) -> grit::FunName {
    for i in 1.. {
      let name = grit::FunName(format!("{}_{}", base, i));
      if self.fun_names.insert(name.clone()) {
        return name
      }
    }
    unreachable!()
  }

  fn gen_obj_name(&mut self, base: &str) -> grit::ObjName {
    for i in 1.. {
      let name = grit::ObjName(format!("{}_{}", base, i));
      if self.obj_names.insert(name.clone()) {
        return name
      }
    }
    unreachable!()
  }
}

impl<'p> FunSt<'p> {
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
  let mut prog_st = ProgSt {
    fun_names: HashSet::new(),
    obj_names: HashSet::new(),
    fun_defs: Vec::new(),
    obj_defs: Vec::new(),
  };

  let main_name = prog_st.gen_fun_name("main");
  { 
    let mut main_st = FunSt {
      prog_st: &mut prog_st,
      var_count: 0,
      blocks: Vec::new(),
      labels: HashSet::new(),
    };
    let start_label = main_st.gen_label("start");
    let env = spine::env::Env::new()
      .bind_conts(vec![(prog.halt_cont.clone(), ContBind::Return)]);
    translate_term(&mut main_st, &env, start_label.clone(), &prog.body);
    main_st.prog_st.fun_defs.push(grit::FunDef {
      name: main_name.clone(),
      capture_count: 0,
      arg_count: 0,
      var_count: main_st.var_count,
      blocks: main_st.blocks,
      start_label: start_label,
    });
  }

  grit::ProgDef {
    fun_defs: prog_st.fun_defs,
    obj_defs: prog_st.obj_defs,
    main_fun: main_name,
  }
}

fn translate_term(st: &mut FunSt, env: &Env, label: grit::Label, term: &spine::Term) {
  match *term {
    spine::Term::Letcont(ref cont_defs, ref body) => 
      translate_letcont(st, env, label, &cont_defs[..], &**body),
    spine::Term::Letfun(ref fun_defs, ref body) => 
      translate_letfun(st, env, label, &fun_defs[..], &**body),
    spine::Term::Letobj(ref obj_def, ref body) =>
      translate_letobj(st, env, label, obj_def, &**body),
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
        .map(|(arg, arg_var)| (arg.clone(), grit::Val::Var(arg_var))).collect());
    translate_term(st, &inner_env, label.clone(), &cont_def.body);
  }

  translate_term(st, &outer_env, label, body)
}

fn translate_letfun(st: &mut FunSt, env: &Env, label: grit::Label,
  fun_defs: &[spine::FunDef], body: &spine::Term)
{
  let body_label = st.gen_label("letfun");
  let fun_vars: Vec<_> = fun_defs.iter().map(|_| st.gen_var()).collect();
  let fun_names: Vec<_> = fun_defs.iter()
    .map(|def| st.prog_st.gen_fun_name(&def.var.0[..]))
    .collect();

  let fun_binds = fun_defs.iter().zip(fun_vars.iter())
    .map(|(fun_def, var)| (fun_def.var.clone(), grit::Val::Var(var.clone())))
    .collect();
  let inner_env = env.bind_vars(fun_binds);

  let alloc_clos = fun_vars.into_iter()
    .zip(fun_defs.iter())
    .zip(fun_names.iter())
    .map(|((fun_var, fun_def), fun_name)| {
      let captures = fun_def.captures.iter()
        .map(|capture| inner_env.lookup_var(capture).unwrap().clone())
        .collect();
      (fun_var, fun_name.clone(), captures)
    }).collect();

  st.blocks.push(grit::Block {
    label: label.clone(),
    ops: vec![grit::Op::AllocClos(alloc_clos)],
    jump: grit::Jump::Goto(body_label.clone()),
  });
         
  translate_term(st, &inner_env, body_label, body);

  for (fun_def, fun_name) in fun_defs.iter().zip(fun_names.into_iter()) {
    translate_fun_def(st.prog_st, fun_name, fun_def)
  }
}

fn translate_letobj(st: &mut FunSt, env: &Env, label: grit::Label,
  obj_def: &spine::ObjDef, body: &spine::Term)
{
  let obj_name = st.prog_st.gen_obj_name(&obj_def.var.0[..]);
  st.prog_st.obj_defs.push(grit::ObjDef {
    name: obj_name.clone(),
    obj: match obj_def.obj {
      spine::Obj::String(ref bytes) => grit::Obj::String(bytes.clone()),
      spine::Obj::Double(number) => grit::Obj::Double(number),
    },
  });

  let inner_env = env.bind_vars(vec![(obj_def.var.clone(), grit::Val::Obj(obj_name))]);
  translate_term(st, &inner_env, label, body);
}

fn translate_call(st: &mut FunSt, env: &Env, label: grit::Label,
  fun: &spine::Val, ret_cont: &spine::ContName, args: &[spine::Val])
{
  let cont_bind = env.lookup_cont(ret_cont).expect("undefined return cont");
  let grit_callee = grit::Callee::Unknown(translate_val(st, env, fun));
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

fn translate_fun_def(prog_st: &mut ProgSt,
  fun_name: grit::FunName, fun_def: &spine::FunDef) 
{
  let ret_bind = (fun_def.ret.clone(), ContBind::Return);
  let arg_binds = fun_def.args.iter().enumerate()
      .map(|(idx, arg)| (arg.clone(), grit::Val::Arg(idx)));
  let capture_binds = fun_def.captures.iter().enumerate()
      .map(|(idx, capture)| (capture.clone(), grit::Val::Capture(idx)));
  let env = spine::env::Env::new()
    .bind_conts(vec![ret_bind])
    .bind_vars(arg_binds.chain(capture_binds).collect());

  let def = {
    let mut st = FunSt {
      prog_st: prog_st,
      var_count: 0,
      blocks: Vec::new(),
      labels: HashSet::new(),
    };
    let start_label = st.gen_label("start");
    translate_term(&mut st, &env, start_label.clone(), &fun_def.body);
    grit::FunDef {
      name: fun_name,
      capture_count: fun_def.captures.len(),
      arg_count: fun_def.args.len(),
      var_count: st.var_count,
      blocks: st.blocks,
      start_label: start_label,
    }
  };
  prog_st.fun_defs.push(def);
}


fn translate_val(_: &mut FunSt, env: &Env, val: &spine::Val) -> grit::Val {
  match *val {
    spine::Val::Int(num) =>
      grit::Val::Int(num),
    spine::Val::Var(ref var) => env.lookup_var(var).unwrap().clone(),
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
