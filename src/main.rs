#![feature(collections, unicode, old_io, box_syntax)]
#![feature(core, unboxed_closures, std_misc)]
#![allow(dead_code)]
mod sexpr;
mod spiral;
mod spine;
mod grit;
mod asm;

pub fn main() {
  use std::old_io as io;
  use std::str;

  let input = io::stdin().read_to_end().unwrap();
  let str = str::from_utf8(&input[..]).unwrap();
  let sexpr = sexpr::parse::parse_sexpr(str).unwrap();
  let spiral = sexpr::to_spiral::prog_from_sexpr(&sexpr).unwrap();
  let spine = spiral::to_spine::spine_from_spiral(&spiral).unwrap();
  let grit = spine::to_grit::grit_from_spine(&spine);
  let asm = grit::to_asm::asm_from_grit(&grit);
  let gas = asm::to_gas::gas_from_asm(&asm);
  println!("{}", gas);
}
