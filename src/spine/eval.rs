use std::collections::{HashMap};
use spine;

pub fn eval(prog: &spine::ProgDef) -> Vec<f32> {
  let mut st = ProgSt {
      steps: 5_000,
      test_output: Vec::new(),
      fun_defs: prog.fun_defs.iter()
        .map(|fun_def| (fun_def.name.clone(), fun_def))
        .collect(),
    };

  let jump = eval_term(&mut st, HashMap::new(), &prog.body);
  assert_eq!(jump.cont, prog.halt_cont);
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

fn eval_term<'g>(st: &mut ProgSt<'g>, mut vars: HashMap<spine::Var, f32>,
  term: &'g spine::Term) -> Jump 
{
  if st.steps <= 0 {
    panic!("eval step limit exceeded");
  } else {
    st.steps = st.steps - 1;
  }

  match *term {
    spine::Term::Letval(ref var, ref expr, ref body) => {
      let value = eval_expr(&vars, expr);
      vars.insert(var.clone(), value);
      eval_term(st, vars, &**body)
    },
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
        .map(|(arg_var, arg_expr)| (arg_var.clone(), eval_expr(&vars, arg_expr)))
        .collect();
      let jump = eval_term(st, inner_vars, &fun_def.body);
      assert_eq!(jump.cont, fun_def.ret);
      assert_eq!(jump.args.len(), 1);
      Jump { cont: ret_cont.clone(), args: jump.args }
    },
    spine::Term::ExternCall(ref ext_name, ref ret_cont, ref args) => {
      if ext_name.0 == "__test_out" {
        assert_eq!(args.len(), 1);
        let value = eval_expr(&vars, &args[0]);
        st.test_output.push(value);
        Jump { cont: ret_cont.clone(), args: vec![0.0] }
      } else {
        panic!("extern call to '{}'", ext_name.0);
      }
    },
    spine::Term::Cont(ref cont_name, ref args) => 
      Jump {
        cont: cont_name.clone(),
        args: args.iter().map(|arg| eval_expr(&vars, arg)).collect(),
      },
    spine::Term::Branch(ref boolexpr, ref then_cont, ref else_cont) =>
      Jump {
        cont: if eval_boolexpr(&vars, boolexpr) { then_cont } else { else_cont }.clone(),
        args: vec![],
      },
  }
}

fn eval_expr(vars: &HashMap<spine::Var, f32>, expr: &spine::Expr) -> f32 {
  use std::mem;
  fn ftu(f: f32) -> u32 { unsafe { mem::transmute::<f32, u32>(f) } }
  fn utf(u: u32) -> f32 { unsafe { mem::transmute::<u32, f32>(u) } }

  match *expr {
    spine::Expr::Binary(ref binop, ref left, ref right) => {
      let l = eval_expr(vars, &**left);
      let r = eval_expr(vars, &**right);
      match *binop {
        spine::Binop::Add => l + r,
        spine::Binop::Sub => l - r,
        spine::Binop::Mul => l * r,
        spine::Binop::Div => l / r,
        spine::Binop::Max => if l > r { l } else { r },
        spine::Binop::Min => if l < r { l } else { r },
        spine::Binop::Bitand => utf(ftu(l) & ftu(r)),
        spine::Binop::Bitor => utf(ftu(l) | ftu(r)),
        spine::Binop::Bitxor => utf(ftu(l) ^ ftu(r)),
        spine::Binop::Bitandn => utf(ftu(l) & (!ftu(r))),
      }
    },
    spine::Expr::Literal(number) => number,
    spine::Expr::Var(ref var) => *vars.get(var).unwrap(),
  }
}

fn eval_boolexpr(vars: &HashMap<spine::Var, f32>, boolexpr: &spine::Boolexpr) -> bool {
  use std::num::Float;

  match *boolexpr {
    spine::Boolexpr::Compare(ref cmp, ref left, ref right) => {
      let l = eval_expr(vars, &**left);
      let r = eval_expr(vars, &**right);
      match *cmp {
        spine::Cmp::Ordered => !l.is_nan() && !r.is_nan(),
        spine::Cmp::Unordered => l.is_nan() || r.is_nan(),
        spine::Cmp::Less => l < r,
        spine::Cmp::LessEq => l <= r,
        spine::Cmp::Eq => l == r,
        spine::Cmp::NotEq => l != r,
        spine::Cmp::GreaterEq => l >= r,
        spine::Cmp::Greater => l > r,
      }
    },
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

