use spine;

#[derive(PartialEq, Debug)]
pub enum Onion {
  Letcont(Vec<spine::ContDef>, Box<Onion>),
  Letjoin(Box<OnionContDef>, Box<spine::Term>),
  Letfun(Vec<spine::FunDef>, Box<Onion>),
  Letobj(spine::ObjDef, Box<Onion>),
  Hole,
}

#[derive(PartialEq, Debug)]
pub struct OnionContDef {
  pub name: spine::ContName,
  pub args: Vec<spine::Var>,
  pub body: Onion,
}

impl Onion {
  pub fn subst_term(self, term: spine::Term) -> spine::Term {
    match self {
      Onion::Letcont(cont_defs, onion) =>
        spine::Term::Letcont(cont_defs, box onion.subst_term(term)),
      Onion::Letjoin(onion_cont_def, body) =>
        spine::Term::Letcont(vec![onion_cont_def.subst_term(term)], body),
      Onion::Letfun(fun_defs, onion) =>
        spine::Term::Letfun(fun_defs, box onion.subst_term(term)),
      Onion::Letobj(obj_defs, onion) =>
        spine::Term::Letobj(obj_defs, box onion.subst_term(term)),
      Onion::Hole =>
        term,
    }
  }

  pub fn subst_onion(self, onion: Onion) -> Onion {
    match self {
      Onion::Letcont(cont_defs, body_onion) =>
        Onion::Letcont(cont_defs, box body_onion.subst_onion(onion)),
      Onion::Letjoin(onion_cont_def, body) =>
        Onion::Letjoin(box onion_cont_def.subst_onion(onion), body),
      Onion::Letfun(fun_defs, body_onion) =>
        Onion::Letfun(fun_defs, box body_onion.subst_onion(onion)),
      Onion::Letobj(obj_defs, body_onion) =>
        Onion::Letobj(obj_defs, box body_onion.subst_onion(onion)),
      Onion::Hole =>
        onion,
    }
  }
}

impl OnionContDef {
  pub fn subst_term(self, term: spine::Term) -> spine::ContDef {
    spine::ContDef {
      name: self.name,
      args: self.args,
      body: self.body.subst_term(term),
    }
  }

  pub fn subst_onion(self, onion: Onion) -> OnionContDef {
    OnionContDef {
      name: self.name,
      args: self.args,
      body: self.body.subst_onion(onion),
    }
  }
}
