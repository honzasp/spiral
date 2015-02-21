#![feature(collections, unicode, io, box_syntax, hash, core, unboxed_closures)]
mod sexpr;
mod spiral;
mod spine;

pub fn main() {
  use std::old_io as io;
  use std::str;

  let input = io::stdin().read_to_end().unwrap();
  let input_str = str::from_utf8(&input[..]).unwrap();
  let input_sexpr = sexpr::parse::parse_sexpr(input_str).unwrap();
  let input_spiral = sexpr::to_spiral::prog_from_sexpr(&input_sexpr).unwrap();
  let input_spine = spiral::to_spine::spine_from_spiral(&input_spiral).unwrap();
  println!("{:?}", input_spine);
  println!("{:?}", spine::check::check_prog(&input_spine));
}
