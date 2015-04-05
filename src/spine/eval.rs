#![cfg(test)]
use std::collections::{HashMap};
use spine;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RtVal {
  Closure(usize),
  Combinator(spine::FunName),
  Int(i32),
  True,
  False,
}

pub fn eval(prog: &spine::ProgDef) -> Vec<RtVal> {
  let halt_cont = spine::ContName("halt".to_string());
  let call_main = spine::Term::Call(
    spine::Val::Combinator(prog.main_fun.clone()),
    halt_cont.clone(),
    vec![]);

  let mut st = ProgSt {
      steps: 5_000,
      test_output: Vec::new(),
      fun_defs: prog.fun_defs.iter()
        .map(|fun_def| (fun_def.name.clone(), fun_def))
        .collect(),
      closures: Vec::new(),
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
  closures: Vec<Closure>,
}

struct Closure {
  fun_name: spine::FunName,
  captures: Vec<RtVal>,
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
    spine::Term::Letclos(ref clos_defs, ref body) => {
      let mut inner_vars = vars;
      for (idx, clos_def) in clos_defs.iter().enumerate() {
        let rt_val = RtVal::Closure(st.closures.len() + idx);
        inner_vars.insert(clos_def.var.clone(), rt_val);
      }

      for clos_def in clos_defs.iter() {
        st.closures.push(Closure {
          fun_name: clos_def.fun_name.clone(),
          captures: clos_def.captures.iter()
            .map(|c| eval_val(&inner_vars, c)).collect(),
        })
      }

      eval_term(st, inner_vars, &**body)
    },
    spine::Term::Call(ref fun, ref ret_cont, ref args) => {
      match eval_val(&vars, fun) {
        RtVal::Closure(clos_idx) => {
          let (fun_def, inner_vars) = {
            let closure = &st.closures[clos_idx];
            let fun_def = *st.fun_defs.get(&closure.fun_name).unwrap();
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
        RtVal::Combinator(ref fun_name) => {
          let fun_def = *st.fun_defs.get(fun_name).unwrap();
          assert_eq!(fun_def.captures.len(), 0);
          let arg_binds = fun_def.args.iter().zip(args.iter())
            .map(|(var, val)| (var.clone(), eval_val(&vars, val)));
          let inner_vars = arg_binds.collect();

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
    spine::Val::Combinator(ref fun_name) => RtVal::Combinator(fun_name.clone()),
    spine::Val::Int(number) => RtVal::Int(number),
    spine::Val::Var(ref var) => vars.get(var).expect("undefined var").clone(),
    spine::Val::True => RtVal::True,
    spine::Val::False => RtVal::False,
    spine::Val::Obj(_) => panic!("objects not implemented"),
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
    assert_eq!(parse_eval("(program main
      (fun main halt () ()
        (letcont ((landpad (ignored) (cont halt (false))))
          (extern-call println landpad 42))))"),
      vec![Int(42)])
  }

  #[test]
  fn test_combinator() {
    assert_eq!(parse_eval("(program main
      (fun fst ret () (a b) 
        (cont ret a))
      (fun snd ret () (a b)
        (cont ret b))
      (fun main halt () ()
        (letcont ((cc1 () (call (combinator fst) cc2 10 20))
                  (cc2 (x) (extern-call println cc3 x))
                  (cc3 (ignore) (call (combinator snd) cc4 40 50))
                  (cc4 (y) (extern-call println cc5 y))
                  (cc5 (ignore) (cont halt (true))))
          (cont cc1))))"),
      vec![Int(10), Int(50)]);
  }

  #[test]
  fn test_closure() {
    assert_eq!(parse_eval("(program main
      (fun print-capture-1 ret (c0 c1) (a0 a1 a2)
        (extern-call println ret c1))
      (fun print-arg-0 ret (c0 c1) (a0 a1 a2)
        (extern-call println ret a0))
      (fun main halt () ()
        (letclos ((clos-1 print-capture-1 10 20)
                  (clos-2 print-capture-1 30 40)
                  (clos-3 print-arg-0 50 60))
          (letcont ((cc1 ( ) (call clos-1 cc2 70 80 90))
                    (cc2 (_) (call clos-2 cc3 100 110 120))
                    (cc3 (_) (call clos-3 halt 130 140 150)))
            (cont cc1)))))"),
      vec![Int(20), Int(40), Int(130)]);
  }
}
