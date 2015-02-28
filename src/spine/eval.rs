use std::collections::{HashMap};
use spine;

pub fn eval(prog: &spine::ProgDef) -> Vec<f32> {
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
  assert_eq!(jump.args.len(), 0);
  st.test_output
}

struct ProgSt<'g> {
  steps: i32,
  test_output: Vec<f32>,
  fun_defs: HashMap<spine::FunName, &'g spine::FunDef>,
}

struct Jump {
  cont: spine::ContName,
  args: Vec<f32>,
}

fn eval_term<'g>(st: &mut ProgSt<'g>, vars: HashMap<spine::Var, f32>,
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

      let binop = |op: fn(f32, f32) -> f32| {
          assert_eq!(args.len(), 2);
          let a = eval_val(&vars, &args[0]);
          let b = eval_val(&vars, &args[1]);
          Jump { cont: ret_cont.clone(), args: vec![op(a, b)] }
        };

      let binop_bool = |op: fn(&f32, &f32) -> bool| {
          assert_eq!(args.len(), 2);
          let a = eval_val(&vars, &args[0]);
          let b = eval_val(&vars, &args[1]);
          Jump { cont: ret_cont.clone(), args: vec![if op(&a, &b) { 1.0 } else { 0.0 }] }
        };

      match &ext_name.0[..] {
         "__test_out" => {
          assert_eq!(args.len(), 1);
          let value = eval_val(&vars, &args[0]);
          st.test_output.push(value);
          Jump { cont: ret_cont.clone(), args: vec![0.0] }
        },
        "spiral_ext_add" => binop(<f32 as ops::Add>::add),
        "spiral_ext_sub" => binop(<f32 as ops::Sub>::sub),
        "spiral_ext_mul" => binop(<f32 as ops::Mul>::mul),
        "spiral_ext_div" => binop(<f32 as ops::Div>::div),
        "spiral_ext_lt" => binop_bool(<f32 as cmp::PartialOrd>::lt),
        "spiral_ext_le" => binop_bool(<f32 as cmp::PartialOrd>::le),
        "spiral_ext_gt" => binop_bool(<f32 as cmp::PartialOrd>::gt),
        "spiral_ext_ge" => binop_bool(<f32 as cmp::PartialOrd>::ge),
        "spiral_ext_eq" => binop_bool(<f32 as cmp::PartialEq>::eq),
        "spiral_ext_ne" => binop_bool(<f32 as cmp::PartialEq>::ne),
        _ =>
          panic!("extern call to '{}'", ext_name.0)
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

fn eval_val(vars: &HashMap<spine::Var, f32>, val: &spine::Val) -> f32 {
  match *val {
    spine::Val::Literal(number) => number,
    spine::Val::Var(ref var) => *vars.get(var).unwrap(),
  }
}

fn eval_boolval(vars: &HashMap<spine::Var, f32>, boolval: &spine::Boolval) -> bool {
  match *boolval {
    spine::Boolval::IsTrue(ref val) =>
      eval_val(vars, val) != 0.0,
    spine::Boolval::IsFalse(ref val) =>
      eval_val(vars, val) == 0.0,
  }
}

#[cfg(test)]
mod test {
  use spine::helpers::*;
  use spine::eval::{eval};

  #[test]
  fn test_output() {
    assert_eq!(eval(&ProgDef {
        fun_defs: vec![],
        halt_cont: cont("halt"),
        body: Letcont(vec![
            ContDef { 
              name: cont("landpad"),
              args: vec![var("ignored")],
              body: Cont(cont("halt"), vec![]),
            }
          ], box ExternCall(ext_name("__test_out"), cont("landpad"), vec![
                Literal(42.0)
              ]))
      }), vec![42.0]);
  }
}

