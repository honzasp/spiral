#[derive(PartialEq, Debug, Clone)]
pub enum Elem {
  List(Vec<Elem>),
  Identifier(String),
  Int(i32),
  Float(f32),
  String(String),
}

