#![cfg(test)]
use std::collections::{HashMap};
use spine;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RtVal {
  Closure(usize),
  Int(i32),
  True,
  False,
}

pub fn eval(prog: &spine::ProgDef) -> Vec<RtVal> {
  let mut st = ProgSt {
      steps: 5_000,
      test_output: Vec::new(),
      closures: Vec::new(),
    };

  let jump = eval_term(&mut st, HashMap::new(), &prog.body);
  assert_eq!(jump.cont, prog.halt_cont);
  assert_eq!(jump.args.len(), 1);
  st.test_output
}

struct ProgSt<'p> {
  steps: i32,
  test_output: Vec<RtVal>,
  closures: Vec<Closure<'p>>,
}

struct Closure<'p> {
  fun_def: &'p spine::FunDef,
  captures: Vec<RtVal>,
}

struct Jump {
  cont: spine::ContName,
  args: Vec<RtVal>,
}

fn eval_term<'p>(st: &mut ProgSt<'p>, vars: HashMap<spine::Var, RtVal>,
  term: &'p spine::Term) -> Jump 
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
            for (arg, value) in cont_def.args.iter().zip(jump.args.into_iter()) {
              inner_vars.insert(arg.clone(), value);
            }
            jump = eval_term(st, inner_vars, &cont_def.body);
            continue 'jumper;
          }
        }
        return jump;
      }
    },
    spine::Term::Letfun(ref fun_defs, ref body) => {
      let mut inner_vars = vars;
      for (idx, fun_def) in fun_defs.iter().enumerate() {
        let rt_val = RtVal::Closure(st.closures.len() + idx);
        inner_vars.insert(fun_def.var.clone(), rt_val);
      }

      for fun_def in fun_defs.iter() {
        st.closures.push(Closure {
          fun_def: fun_def,
          captures: fun_def.captures.iter()
            .map(|v| inner_vars.get(v).unwrap().clone()).collect(),
        })
      }

      eval_term(st, inner_vars, &**body)
    },
    spine::Term::Letobj(_, _) =>
      panic!("letobj not implemented"),
    spine::Term::Call(ref fun, ref ret_cont, ref args) => {
      match eval_val(&vars, fun) {
        RtVal::Closure(clos_idx) => {
          let (fun_def, inner_vars) = {
            let closure = &st.closures[clos_idx];
            let fun_def = closure.fun_def;
            assert_eq!(fun_def.args.len(), args.len());
            assert_eq!(fun_def.captures.len(), closure.captures.len());

            let arg_binds = fun_def.args.iter().zip(args.iter())
              .map(|(var, val)| (var.clone(), eval_val(&vars, val)));
            let capture_binds = fun_def.captures.iter().zip(closure.captures.iter())
              .map(|(var, rt_val)| (var.clone(), rt_val.clone()));
            let inner_vars = arg_binds.chain(capture_binds).collect();
            (fun_def, inner_vars)
          };

          let jump = eval_term(st, inner_vars, &fun_def.body);
          assert_eq!(jump.cont, fun_def.ret);
          assert_eq!(jump.args.len(), 1);
          Jump { cont: ret_cont.clone(), args: jump.args }
        },
        other => panic!("cannot call {:?}", other),
      }
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
            (a, b) => panic!("expected ints to binop, got {:?} and {:?}", a, b),
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
            (a, b) => panic!("expected ints to cmp, got {:?} and {:?}", a, b),
          }
        };

      match &ext_name.0[..] {
        "spiral_std_println" | "println" => {
          assert_eq!(args.len(), 1);
          let value = eval_val(&vars, &args[0]);
          st.test_output.push(value);
          Jump { cont: ret_cont.clone(), args: vec![RtVal::False] }
        },
        "spiral_std_add" => binop(<i32 as ops::Add>::add),
        "spiral_std_sub" => binop(<i32 as ops::Sub>::sub),
        "spiral_std_mul" => binop(<i32 as ops::Mul>::mul),
        "spiral_std_div" => binop(<i32 as ops::Div>::div),
        "spiral_std_lt" => binop_bool(<i32 as cmp::PartialOrd>::lt),
        "spiral_std_le" => binop_bool(<i32 as cmp::PartialOrd>::le),
        "spiral_std_gt" => binop_bool(<i32 as cmp::PartialOrd>::gt),
        "spiral_std_ge" => binop_bool(<i32 as cmp::PartialOrd>::ge),
        "spiral_std_eq" => binop_bool(<i32 as cmp::PartialEq>::eq),
        "spiral_std_ne" => binop_bool(<i32 as cmp::PartialEq>::ne),
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
    spine::Val::Var(ref var) => vars.get(var).expect("undefined var").clone(),
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
  use sexpr;
  use spine;
  use spine::eval::RtVal::{self, Int};

  fn parse_eval(txt: &str) -> Vec<RtVal> {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    let spine = sexpr::to_spine::prog_from_sexpr(&sexpr).unwrap();
    spine::eval::eval(&spine)
  }

  #[test]
  fn test_output() {
    assert_eq!(parse_eval("(program halt
        (extern-call println halt 42))"),
      vec![Int(42)])
  }

  #[test]
  fn test_closure() {
    assert_eq!(parse_eval("(program halt
      (letfun (ff-1 r () (a b)
                (letfun (ff-2 q (a) () (extern-call println q a))
                  (call ff-2 r)))
        (call ff-1 halt 10 20)))"),
      vec![Int(10)]);
  }
}
