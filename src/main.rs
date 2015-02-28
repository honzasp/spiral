#![feature(collections, unicode, old_io, box_syntax)]
#![feature(core, unboxed_closures)]
mod sexpr;
mod spiral;
mod spine;
mod grit;

pub fn main() {
  use std::old_io as io;
  use std::str;

  let input = io::stdin().read_to_end().unwrap();
  let str = str::from_utf8(&input[..]).unwrap();
  let sexpr = sexpr::parse::parse_sexpr(str).unwrap();
  let spiral = sexpr::to_spiral::prog_from_sexpr(&sexpr).unwrap();
  let spine = spiral::to_spine::spine_from_spiral(&spiral).unwrap();
  let grit = spine::to_grit::grit_from_spine(&spine);
  println!("{:?}", grit);
  println!("{:?}", spine::check::check_prog(&spine));
}
