use grit;

#[derive(PartialEq, Debug)]
pub struct SlotAlloc {
  pub slot_count: usize,
  pub vars: Vec<grit::Slot>,
}

pub fn alloc(fun_def: &grit::FunDef) -> SlotAlloc {
  SlotAlloc {
    slot_count: fun_def.arg_count + fun_def.var_count,
    vars: (0..fun_def.var_count).map(|i| grit::Slot(fun_def.arg_count + i)).collect(),
  }
}
