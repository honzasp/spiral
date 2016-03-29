#![feature(box_syntax, str_char, iter_arith)]
extern crate bit_set;
extern crate docopt;
extern crate rustc_serialize;
extern crate tempdir;

use std::convert;

mod args;
mod sexpr;
mod spiral;
mod spine;
mod grit;
mod asm;
mod map_in_place;

static VERSION: &'static str = "0.0.1";

#[derive(Debug, Clone)]
struct SpiralError(String);

fn main_body() -> Result<(), SpiralError> {
  use std::io::prelude::*;
  use std::process;
  use std::path;
  use std::fs;
  use std::env;
  use args::{Emit, parse_args};

  let os_args: Vec<_> = env::args().collect();
  let args = parse_args(os_args.iter().map(|arg| &arg[..]).collect())
    .unwrap_or_else(|e| e.exit());
  let opt_level = args.flag_opt_level.unwrap_or(2);

  let output_path = match args.flag_output {
    Some(ref path) => path::PathBuf::from(path),
    None => {
      let ext = match args.flag_emit {
        Some(Emit::Sexpr) => "sexpr",
        Some(Emit::Spiral) => "spiral",
        Some(Emit::Spine) => "spine",
        Some(Emit::Grit) => "grit",
        Some(Emit::Asm) => "txt",
        Some(Emit::Gas) => "s",
        Some(Emit::Exec) | None => "",
      };
      let mut path = path::PathBuf::from(&args.arg_input);
      path.set_extension(ext);
      path
    },
  };

  let dump = |txt: String| -> Result<(), SpiralError> {
    let mut output = try!(fs::File::create(&output_path));
    try!(output.write_all(txt.as_bytes()));
    Ok(())
  };

  let dump_sexpr = |elem: &sexpr::Elem| -> Result<(), SpiralError> {
    let mut output = try!(fs::File::create(&output_path));
    try!(output.write_all(sexpr::pretty_print::pretty_print_sexpr(elem).as_bytes()));
    try!(output.write_all(b"\n"));
    Ok(())
  };

  let parse_sexpr = |file: &mut fs::File| -> Result<sexpr::Elem, String> {
    let mut input_str = String::new();
    match file.read_to_string(&mut input_str) {
      Ok(_) => sexpr::parse::parse_sexpr(&input_str[..]),
      Err(e) => Err(format!("error reading sexpr: {}", e)),
    }
  };

  let sexpr = try!(parse_sexpr(&mut try!(fs::File::open(&args.arg_input))));
  if args.flag_emit == Some(Emit::Sexpr) {
    return dump_sexpr(&sexpr);
  }

  let spiral = try!(sexpr::to_spiral::prog_from_sexpr(&sexpr));
  if args.flag_emit == Some(Emit::Spiral) {
    return dump(format!("{:?}", sexpr))
  }

  let mut mod_loader = |mod_name: &spiral::ModName| {
    for path in args.flag_include.iter() {
      let mut path_buf = path::PathBuf::from(path);
      path_buf.push(format!("{}.spiral", mod_name.0));
      match fs::File::open(&path_buf) {
        Ok(mut file) => {
          let sexpr = try!(parse_sexpr(&mut file));
          return sexpr::to_spiral::mod_from_sexpr(&sexpr)
        },
        Err(_) => (),
      }
    }
    Err(format!("module '{}' was not found", mod_name.0))
  };

  let spine = try!(spiral::to_spine::spine_from_spiral(&spiral, &mut mod_loader));
  let spine_errs = spine::check::check(&spine);
  assert_eq!(spine_errs, Vec::<String>::new());
  if args.flag_emit == Some(Emit::Spine) {
    return dump_sexpr(&spine::to_sexpr::prog_to_sexpr(&spine));
  }

  let grit = spine::to_grit::grit_from_spine(&spine);
  let grit = optimize_grit(grit, opt_level);
  if args.flag_emit == Some(Emit::Grit) {
    return dump_sexpr(&grit::to_sexpr::prog_to_sexpr(&grit));
  }

  let asm = grit::to_asm::asm_from_grit(&grit);
  let asm = optimize_asm(asm, opt_level);
  if args.flag_emit == Some(Emit::Asm) {
    return dump(format!("{:?}", asm))
  }

  let gas = asm::to_gas::gas_from_asm(&asm);
  if args.flag_emit == Some(Emit::Gas) {
    return dump(gas)
  }

  let dir = try!(tempdir::TempDir::new("spiral"));
  let asm_file = {
    let mut buf = path::PathBuf::from(dir.path());
    buf.push("assembly.s");
    buf
  };
  let obj_file = {
    let mut buf = path::PathBuf::from(dir.path());
    buf.push("object.o");
    buf
  };
  try!(try!(fs::File::create(&asm_file)).write_all(gas.as_bytes()));

  let gas_cmd = args.flag_gas_cmd.clone().unwrap_or("as".to_string());
  let gas_out = try!(process::Command::new(&gas_cmd[..])
    .arg(&asm_file)
    .arg("-o").arg(&obj_file)
    .arg("-march=i386").arg("--32")
    .output());
  if !gas_out.status.success() {
    try!(Err(format!("Assembler failed:\n{}", String::from_utf8_lossy(&gas_out.stderr))))
  }

  let runtime_path = args.flag_runtime.clone().unwrap_or("runtime.a".to_string());
  let link_cmd = args.flag_link_cmd.clone().unwrap_or("clang".to_string());
  let link_out = try!(process::Command::new(&link_cmd[..])
    .arg(&obj_file)
    .arg(&runtime_path[..])
    .arg("-o").arg(&output_path)
    .arg("-lm")
    .arg("-march=i386").arg("-m32")
    .output());
  if !link_out.status.success() {
    try!(Err(format!("Linker failed:\n{}", String::from_utf8_lossy(&link_out.stderr))))
  }

  Ok(())
}

fn optimize_grit(mut grit: grit::ProgDef, level: u8) -> grit::ProgDef {
  if level >= 1 {
    grit = grit::optimize_values::optimize(grit);
    grit = grit::optimize_dead_vals::optimize(grit);
  } 

  if level >= 2 {
    grit = grit::optimize_inline::optimize(grit);
    if level >= 3 {
      grit = grit::optimize_values::optimize(grit);
      grit = grit::optimize_dead_vals::optimize(grit);
    }
  }

  if level >= 1 {
    grit = grit::optimize_dead_defs::optimize(grit);
  }

  grit
}

fn optimize_asm(mut asm: asm::ProgDef, level: u8) -> asm::ProgDef {
  if level >= 1 {
    asm = asm::simplify::simplify_prog(asm);
  }

  asm
}

pub fn main() {
  use std::io;
  use std::io::prelude::*;

  match main_body() {
    Ok(()) => (),
    Err(msg) => writeln!(&mut io::stderr(), "Error: {}", msg.0).unwrap(),
  }
}

impl convert::From<std::io::Error> for SpiralError {
  fn from(err: std::io::Error) -> SpiralError {
    SpiralError(format!("{}", err))
  }
}

impl convert::From<String> for SpiralError {
  fn from(err: String) -> SpiralError {
    SpiralError(err)
  }
}

