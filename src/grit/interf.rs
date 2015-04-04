use std::collections::{HashMap, HashSet};
use grit;

#[derive(PartialEq, Debug)]
pub struct Graph {
  pub vars: Vec<HashSet<grit::Var>>,
  pub slots: Vec<HashSet<grit::Slot>>,
}

pub fn build_graph(fun_def: &grit::FunDef) -> Graph {
  fn add_clique(graph: &mut Graph, vars: HashSet<grit::Var>, slots: HashSet<grit::Slot>) {
    for var in vars.iter() {
      for interf_var in vars.iter() {
        if var != interf_var {
          graph.vars[var.0].insert(interf_var.clone());
        }
      }

      for interf_slot in slots.iter() {
        graph.slots[var.0].insert(interf_slot.clone());
      }
    }
  }

  let jumps = collect_jumps(fun_def);
  let fun_cliques = collect_cliques(fun_def, &jumps);
  let mut graph = Graph {
    vars: (0..fun_def.var_count).map(|_| HashSet::new()).collect(),
    slots: (0..fun_def.var_count).map(|_| HashSet::new()).collect(),
  };

  for (_label, cliques) in fun_cliques.into_iter() {
    for (vars, slots) in cliques.vars.into_iter().zip(cliques.slots.into_iter()) {
      add_clique(&mut graph, vars, slots);
    }
  }
  graph
}

struct Cliques {
  vars: Vec<HashSet<grit::Var>>,
  slots: Vec<HashSet<grit::Slot>>,
}

fn collect_cliques<'p>(fun_def: &'p grit::FunDef,
  jumps: &HashMap<&'p grit::Label, HashSet<&'p grit::Label>>)
  -> HashMap<&'p grit::Label, Cliques>
{
  let mut fun_cliques = fun_def.blocks.iter().map(|block| {
    let mut vars: Vec<_> = (0..block.ops.len()+1).map(|_| HashSet::new()).collect();
    for (idx, op) in block.ops.iter().enumerate() {
      let (writes, is_prewrite) = op_writes(op);
      if is_prewrite {
        vars[idx].extend(writes.into_iter());
      } else {
        vars[idx + 1].extend(writes.into_iter());
      }
    }

    (&block.label, Cliques {
      vars: vars,
      slots: (0..block.ops.len()+1).map(|_| HashSet::new()).collect(),
    })
  }).collect();

  for block in fun_def.blocks.iter() {
    for (idx, op) in block.ops.iter().enumerate() {
      let (read_vars, read_slots) = op_reads(op);
      mark_needed(&mut fun_cliques, jumps,
        read_vars, read_slots,
        &block.label, Some(idx));
    }

    {
      let (read_vars, read_slots) = jump_reads(&block.jump);
      mark_needed(&mut fun_cliques, jumps,
        read_vars, read_slots,
        &block.label, None);
    }
  }

  fun_cliques
}

fn mark_needed<'p>(
  fun_cliques: &mut HashMap<&'p grit::Label, Cliques>,
  jumps: &HashMap<&'p grit::Label, HashSet<&'p grit::Label>>,
  mut vars: Vec<grit::Var>, mut slots: Vec<grit::Slot>,
  label: &'p grit::Label, idx: Option<usize>)
{
  { let block_cliques = fun_cliques.get_mut(label).unwrap();
    let last_idx = match idx {
      Some(idx) => idx + 1,
      None => block_cliques.vars.len(),
    };

    for i in (0..last_idx).rev() {
      vars = vars.into_iter().filter(|var| {
          block_cliques.vars[i].insert(var.clone())
        }).collect();
      slots = slots.into_iter().filter(|slot| {
          block_cliques.slots[i].insert(slot.clone())
        }).collect();
    }
  }

  if !vars.is_empty() || !slots.is_empty() {
    let in_jumps = jumps.get(label).unwrap();
    for in_jump in in_jumps.iter() {
      mark_needed(fun_cliques, jumps, 
        vars.clone(), slots.clone(),
        in_jump, None)
    }

    if in_jumps.is_empty() && !vars.is_empty() {
      panic!("reads from {:?} at {:?}/{:?} are not dominated by any write",
        vars, label, idx);
    }
  }
}

fn op_reads(op: &grit::Op) -> (Vec<grit::Var>, Vec<grit::Slot>) {
  let mut var_reads = Vec::new();
  let mut slot_reads = Vec::new();
  match *op {
    grit::Op::Call(_, ref callee, ref args) => {
      add_callee_reads(&mut var_reads, &mut slot_reads, callee);
      for arg in args.iter() {
        add_val_reads(&mut var_reads, &mut slot_reads, arg);
      }
    },
    grit::Op::ExternCall(_, _, ref args) =>
      for arg in args.iter() {
        add_val_reads(&mut var_reads, &mut slot_reads, arg);
      },
    grit::Op::AllocClos(ref closs) => 
      for &(_, _, ref captures) in closs.iter() {
        for capture in captures.iter() {
          add_val_reads(&mut var_reads, &mut slot_reads, capture);
        }
      },
    grit::Op::Assign(ref var_vals) =>
      for &(_, ref val) in var_vals.iter() {
        add_val_reads(&mut var_reads, &mut slot_reads, val);
      },
  }
  (var_reads, slot_reads)
}

fn jump_reads(jump: &grit::Jump) -> (Vec<grit::Var>, Vec<grit::Slot>) {
  let mut var_reads = Vec::new();
  let mut slot_reads = Vec::new();
  match *jump {
    grit::Jump::Goto(_) => { },
    grit::Jump::Return(ref val) =>
      add_val_reads(&mut var_reads, &mut slot_reads, val),
    grit::Jump::TailCall(ref callee, ref args) => {
      add_callee_reads(&mut var_reads, &mut slot_reads, callee);
      for arg in args.iter() {
        add_val_reads(&mut var_reads, &mut slot_reads, arg);
      }
    },
    grit::Jump::Branch(ref boolval, _, _) =>
      add_boolval_reads(&mut var_reads, &mut slot_reads, boolval),
  }
  (var_reads, slot_reads)
}

fn add_boolval_reads(var_reads: &mut Vec<grit::Var>,
  slot_reads: &mut Vec<grit::Slot>, boolval: &grit::Boolval)
{
  match *boolval {
    grit::Boolval::IsTrue(ref val) |
    grit::Boolval::IsFalse(ref val) =>
      add_val_reads(var_reads, slot_reads, val),
  }
}


fn add_callee_reads(var_reads: &mut Vec<grit::Var>,
  slot_reads: &mut Vec<grit::Slot>, callee: &grit::Callee)
{
  match *callee {
    grit::Callee::KnownClosure(_, ref val) |
    grit::Callee::Unknown(ref val) =>
      add_val_reads(var_reads, slot_reads, val),
    grit::Callee::Combinator(_) => (),
  }
}

fn add_val_reads(var_reads: &mut Vec<grit::Var>,
  slot_reads: &mut Vec<grit::Slot>, val: &grit::Val)
{
  match *val {
    grit::Val::Var(ref var) => { var_reads.push(var.clone()); },
    grit::Val::Arg(slot_idx) => { slot_reads.push(grit::Slot(slot_idx)); },
    _ => { },
  }
}

fn op_writes(op: &grit::Op) -> (HashSet<grit::Var>, bool) {
  let mut writes = HashSet::new();
  let mut is_prewrite = false;
  match *op {
    grit::Op::Call(ref var, _, _) |
    grit::Op::ExternCall(ref var, _, _) => {
      writes.insert(var.clone());
    },
    grit::Op::AllocClos(ref closs) => {
      for &(ref var, _, _) in closs.iter() {
        writes.insert(var.clone());
      }
      is_prewrite = true;
    },
    grit::Op::Assign(ref var_vals) =>
      for &(ref var, _) in var_vals.iter() {
        writes.insert(var.clone());
      },
  }
  (writes, is_prewrite)
}

fn collect_jumps<'p>(fun_def: &'p grit::FunDef) 
  -> HashMap<&'p grit::Label, HashSet<&'p grit::Label>>
{
  let mut jumps: HashMap<_, _> = fun_def.blocks.iter().map(|block| {
      (&block.label, HashSet::new())
    }).collect();

  for block in fun_def.blocks.iter() {
    match block.jump {
      grit::Jump::Goto(ref target) => {
        jumps.get_mut(target).unwrap().insert(&block.label);
      },
      grit::Jump::Return(_) => { },
      grit::Jump::TailCall(..) => { },
      grit::Jump::Branch(_, ref then_label, ref else_label) => {
        jumps.get_mut(then_label).unwrap().insert(&block.label);
        jumps.get_mut(else_label).unwrap().insert(&block.label);
      },
    }
  }

  jumps
}

#[cfg(test)]
mod test {
  use std::collections::{HashSet};
  use sexpr;
  use grit;

  fn interf_graph(txt: &str) -> grit::interf::Graph {
    let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
    let grit = sexpr::to_grit::fun_def_from_sexpr(&sexpr).unwrap();
    grit::interf::build_graph(&grit)
  }

  fn build_graph(var_count: usize, vars: Vec<(usize, usize)>,
    slots: Vec<(usize, usize)>) -> grit::interf::Graph
  {
    let mut graph = grit::interf::Graph {
      vars: (0..var_count).map(|_| HashSet::new()).collect(),
      slots: (0..var_count).map(|_| HashSet::new()).collect(),
    };
    for (var_a, var_b) in vars.into_iter() {
      graph.vars[var_a].insert(grit::Var(var_b));
      graph.vars[var_b].insert(grit::Var(var_a));
    }
    for (var_a, slot_b) in slots.into_iter() {
      graph.slots[var_a].insert(grit::Slot(slot_b));
    }
    graph
  }

  #[test]
  fn test_one_block_interf() {
    assert_eq!(
      interf_graph("(fun f 0 1 6 start
        (start
          (assign (0 (int 10)) (1 (int 20)))
          (call 2 (combinator xy) (arg 0))
          (call 3 (combinator yz) (var 2) (var 0))
          (assign (4 (var 3)) (5 (var 2)))
          (return (var 5))))"),
      build_graph(6, vec![
          (0, 1),
          (4, 5),
          (2, 3),
          (0, 2),
        ], vec![
          (0, 0),
          (1, 0),
        ]));
  }

  #[test]
  fn test_multiple_block_interf() {
    assert_eq!(
      interf_graph("(fun f 0 2 8 start
        (start
          (assign (0 (arg 0)) (1 (arg 1)))
          (call 2 (combinator ff))
          (call 3 (combinator ff))
          (branch (is-true (var 1)) left right))
        (left
          (call 4 (combinator ff) (var 2))
          (call 5 (combinator ff) (var 3))
          (goto join))
        (right
          (call 4 (combinator ff) (var 0))
          (alloc-clos (5 gg (var 4) (var 0)))
          (goto join))
        (join
          (assign (6 (var 5)))
          (assign (7 (arg 1)))
          (return (var 7))))"),
      build_graph(8, vec![
          (0, 1),
          (0, 2),
          (1, 3),
          (1, 2),
          (3, 2),
          (3, 0),
          (3, 4),
          (0, 4),
          (5, 4),
          (5, 0),
        ], vec![
          (6, 1),
          (5, 1),
          (4, 1),
          (3, 1),
          (2, 1),
          (0, 1),
          (1, 1),
        ]));
  }

  #[test]
  fn test_alloc_interf() {
    assert_eq!(
      interf_graph("(fun f 0 1 4 start
        (start
          (assign (1 (int 10)))
          (assign (0 (int 20)))
          (alloc-clos
            (1 f1 (var 1) (var 2))
            (2 f2 (var 0))
            (3 f3 (var 3)))
          (return (int 0))))"),
        build_graph(4, vec![
          (1, 2),
          (1, 3),
          (2, 3),
          (0, 1),
          (0, 2),
          (0, 3),
        ], vec![]));
  }
}
