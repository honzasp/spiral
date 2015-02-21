#![feature(collections, unicode, io, box_syntax, hash, core, unboxed_closures)]
mod sexpr;
mod spiral;
mod spiralize;
mod spine;
mod ossify;

pub fn main() {
  use std::old_io as io;
  use std::str;
  use sexpr;
  use spiralize;

  let input = io::stdin().read_to_end().unwrap();
  let input_str = str::from_utf8(&input[..]).unwrap();
  let input_sexpr = sexpr::parse_sexpr(input_str).unwrap();
  let input_spiral = spiralize::prog_from_sexpr(&input_sexpr).unwrap();
  let input_spine = ossify::spine_from_spiral(&input_spiral).unwrap();
  println!("{:?}", input_spine);
}
