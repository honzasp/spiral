#[derive(PartialEq, Debug, Clone)]
pub enum Elem {
  List(Vec<Elem>),
  Identifier(String),
  Int(i32),
  Double(f64),
  String(String),
}

