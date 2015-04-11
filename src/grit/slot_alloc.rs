use std::collections::{HashSet};
use grit;

#[derive(PartialEq, Debug)]
pub struct SlotAlloc {
  pub slot_count: usize,
  pub vars: Vec<grit::Slot>,
}

pub fn alloc(fun_def: &grit::FunDef) -> SlotAlloc {
  let graph = grit::interf::build_graph(fun_def);
  let order = build_order(fun_def, &graph);

  let mut color_count = fun_def.arg_count;
  let mut coloring: Vec<Option<grit::Slot>> = (0..fun_def.var_count)
    .map(|_| None).collect();

  for var in order.iter() {
    let neigh_set: HashSet<grit::Slot> = {
      let var_neighs = graph.vars.get(var.0).unwrap().iter()
        .filter_map(|var_neigh| coloring[var_neigh.0].clone());
      let slot_neighs = graph.slots.get(var.0).unwrap().iter().cloned();
      var_neighs.chain(slot_neighs).collect()
    };

    for color in (0..) {
      if !neigh_set.contains(&grit::Slot(color)) {
        assert_eq!(coloring[var.0], None);
        coloring[var.0] = Some(grit::Slot(color));
        if color + 1 > color_count {
          color_count = color + 1;
        }
        break
      }
    }
  }

  SlotAlloc {
    slot_count: color_count,
    vars: coloring.into_iter().map(|opt_color| opt_color.unwrap()).collect(),
  }
}

fn build_order(fun_def: &grit::FunDef, graph: &grit::interf::Graph) -> Vec<grit::Var> {
  let mut order: Vec<_> = (0..fun_def.var_count).map(grit::Var).collect();
  order.sort_by(|var_a: &grit::Var, var_b: &grit::Var| {
    let degree_a = graph.vars[var_a.0].len() + graph.slots[var_a.0].len();
    let degree_b = graph.vars[var_b.0].len() + graph.slots[var_b.0].len();
    degree_b.cmp(&degree_a)
  });
  order
}
