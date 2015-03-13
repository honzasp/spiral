use std::collections::{HashMap};
use spine;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RtVal {
  Int(i32),
  True,
  False,
}

pub fn eval(prog: &spine::ProgDef) -> Vec<RtVal> {
  let halt_cont = spine::ContName("halt".to_string());
  let call_main = spine::Term::Call(prog.main_fun.clone(), halt_cont.clone(), vec![]);
  let mut st = ProgSt {
      steps: 5_000,
      test_output: Vec::new(),
      fun_defs: prog.fun_defs.iter()
        .map(|fun_def| (fun_def.name.clone(), fun_def))
        .collect(),
    };

  let jump = eval_term(&mut st, HashMap::new(), &call_main);
  assert_eq!(jump.cont, halt_cont);
  assert_eq!(jump.args.len(), 1);
  st.test_output
}

struct ProgSt<'g> {
  steps: i32,
  test_output: Vec<RtVal>,
  fun_defs: HashMap<spine::FunName, &'g spine::FunDef>,
}

struct Jump {
  cont: spine::ContName,
  args: Vec<RtVal>,
}

fn eval_term<'g>(st: &mut ProgSt<'g>, vars: HashMap<spine::Var, RtVal>,
  term: &'g spine::Term) -> Jump 
{
  if st.steps <= 0 {
    panic!("eval step limit exceeded");
  } else {
    st.steps = st.steps - 1;
  }

  match *term {
    spine::Term::Letcont(ref cont_defs, ref body) => {
      let mut jump = eval_term(st, vars.clone(), &**body);
      'jumper: loop {
        for cont_def in cont_defs.iter() {
          if cont_def.name == jump.cont {
            assert_eq!(cont_def.args.len(), jump.args.len());
            let mut inner_vars = vars.clone();
            for (arg, &value) in cont_def.args.iter().zip(jump.args.iter()) {
              inner_vars.insert(arg.clone(), value);
            }
            jump = eval_term(st, inner_vars, &cont_def.body);
            continue 'jumper;
          }
        }
        return jump;
      }
    },
    spine::Term::Call(ref fun_name, ref ret_cont, ref args) => {
      let fun_def = *st.fun_defs.get(fun_name).unwrap();
      let inner_vars = fun_def.args.iter().zip(args.iter())
        .map(|(arg_var, arg_val)| (arg_var.clone(), eval_val(&vars, arg_val)))
        .collect();
      let jump = eval_term(st, inner_vars, &fun_def.body);
      assert_eq!(jump.cont, fun_def.ret);
      assert_eq!(jump.args.len(), 1);
      Jump { cont: ret_cont.clone(), args: jump.args }
    },
    spine::Term::ExternCall(ref ext_name, ref ret_cont, ref args) => {
      use std::{ops, cmp};

      let binop = |op: fn(i32, i32) -> i32| {
          assert_eq!(args.len(), 2);
          let a = eval_val(&vars, &args[0]);
          let b = eval_val(&vars, &args[1]);
          match (a, b) {
            (RtVal::Int(i), RtVal::Int(j)) =>
              Jump { cont: ret_cont.clone(), args: vec![RtVal::Int(op(i, j))] },
            _ => panic!("expected ints to binop, got {:?} and {:?}", a, b),
          }
        };

      let binop_bool = |op: fn(&i32, &i32) -> bool| {
          assert_eq!(args.len(), 2);
          let a = eval_val(&vars, &args[0]);
          let b = eval_val(&vars, &args[1]);
          match (a, b) {
            (RtVal::Int(i), RtVal::Int(j)) => {
              let res = if op(&i, &j) { RtVal::True } else { RtVal::False };
              Jump { cont: ret_cont.clone(), args: vec![res] }
            },
            _ => panic!("expected ints to cmp, got {:?} and {:?}", a, b),
          }
        };

      match &ext_name.0[..] {
         "__test_out" => {
          assert_eq!(args.len(), 1);
          let value = eval_val(&vars, &args[0]);
          st.test_output.push(value);
          Jump { cont: ret_cont.clone(), args: vec![RtVal::False] }
        },
        "spiral_ext_add" => binop(<i32 as ops::Add>::add),
        "spiral_ext_sub" => binop(<i32 as ops::Sub>::sub),
        "spiral_ext_mul" => binop(<i32 as ops::Mul>::mul),
        "spiral_ext_div" => binop(<i32 as ops::Div>::div),
        "spiral_ext_lt" => binop_bool(<i32 as cmp::PartialOrd>::lt),
        "spiral_ext_le" => binop_bool(<i32 as cmp::PartialOrd>::le),
        "spiral_ext_gt" => binop_bool(<i32 as cmp::PartialOrd>::gt),
        "spiral_ext_ge" => binop_bool(<i32 as cmp::PartialOrd>::ge),
        "spiral_ext_eq" => binop_bool(<i32 as cmp::PartialEq>::eq),
        "spiral_ext_ne" => binop_bool(<i32 as cmp::PartialEq>::ne),
        _ => panic!("extern call to '{}'", ext_name.0)
      }
    },
    spine::Term::Cont(ref cont_name, ref args) => 
      Jump {
        cont: cont_name.clone(),
        args: args.iter().map(|arg| eval_val(&vars, arg)).collect(),
      },
    spine::Term::Branch(ref boolval, ref then_cont, ref else_cont) =>
      Jump {
        cont: if eval_boolval(&vars, boolval) { then_cont } else { else_cont }.clone(),
        args: vec![],
      },
  }
}

fn eval_val(vars: &HashMap<spine::Var, RtVal>, val: &spine::Val) -> RtVal {
  match *val {
    spine::Val::Int(number) => RtVal::Int(number),
    spine::Val::Var(ref var) => *vars.get(var).unwrap(),
    spine::Val::True => RtVal::True,
    spine::Val::False => RtVal::False,
  }
}

fn eval_boolval(vars: &HashMap<spine::Var, RtVal>, boolval: &spine::Boolval) -> bool {
  match *boolval {
    spine::Boolval::IsTrue(ref val) =>
      eval_val(vars, val) != RtVal::False,
    spine::Boolval::IsFalse(ref val) =>
      eval_val(vars, val) == RtVal::False,
  }
}

#[cfg(test)]
mod test {
  use spine::helpers::*;
  use spine::eval::{eval, RtVal};

  #[test]
  fn test_output() {
    assert_eq!(eval(&ProgDef {
        main_fun: fun("main"),
        fun_defs: vec![
          FunDef {
            name: fun("main"),
            ret: cont("halt"),
            args: vec![],
            body: Letcont(vec![
              ContDef { 
                name: cont("landpad"),
                args: vec![var("ignored")],
                body: Cont(cont("halt"), vec![False]),
              }
            ], box ExternCall(ext_name("__test_out"), cont("landpad"), vec![
                  Int(42)
                ]))
          }
        ]}), vec![RtVal::Int(42)]);
  }
}

