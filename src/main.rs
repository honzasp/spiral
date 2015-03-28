#![allow(stable_features, deprecated)]
#![feature(collections, box_syntax)]
#![feature(core, unboxed_closures)]
#![feature(process)]
#![feature(str_char)]
#![feature(convert)]
extern crate docopt;
extern crate rustc_serialize;
extern crate tempdir;

mod args;
mod sexpr;
mod spiral;
mod spine;
mod grit;
mod asm;

#[derive(Debug, Clone)]
struct SpiralError(String);

fn main_body() -> Result<(), SpiralError> {
  use std::io::prelude::*;
  use std::process;
  use std::path;
  use std::fs;
  use args::{Emit, parse_args};

  let args = parse_args();

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
          return sexpr::to_spiral::mod_from_sexpr(&sexpr);
        },
        Err(_) => (),
      }
    }
    Err(format!("module '{}' was not found", mod_name.0))
  };
  let spine = try!(spiral::to_spine::spine_from_spiral(&spiral, &mut mod_loader));
  if args.flag_emit == Some(Emit::Spine) {
    return dump_sexpr(&spine::to_sexpr::prog_to_sexpr(&spine));
  }

  let grit = spine::to_grit::grit_from_spine(&spine);
  if args.flag_emit == Some(Emit::Grit) {
    return dump_sexpr(&grit::to_sexpr::prog_to_sexpr(&grit));
  }

  let asm = grit::to_asm::asm_from_grit(&grit);
  if args.flag_emit == Some(Emit::Asm) {
    return dump(format!("{:?}", asm))
  }

  let gas = asm::to_gas::gas_from_asm(&asm);
  if args.flag_emit == Some(Emit::Gas) {
    return dump(gas)
  }

  try!(try!(fs::File::create("/tmp/assembly.s")).write_all(gas.as_bytes()));

  let gas_cmd = args.flag_gas_cmd.clone().unwrap_or("as".to_string());
  let gas_out = try!(process::Command::new(&gas_cmd[..])
    .arg("/tmp/assembly.s")
    .arg("-o").arg("/tmp/object.o")
    .output());
  if !gas_out.status.success() {
    try!(Err(format!("Assembler failed:\n{}", String::from_utf8_lossy(&gas_out.stderr))))
  }

  let runtime_path = args.flag_runtime.clone().unwrap_or("runtime.a".to_string());
  let link_cmd = args.flag_link_cmd.clone().unwrap_or("clang".to_string());
  let link_out = try!(process::Command::new(&link_cmd[..])
    .arg("/tmp/object.o")
    .arg(&runtime_path[..])
    .arg("-o").arg(&output_path)
    .output());
  if !link_out.status.success() {
    try!(Err(format!("Linker failed:\n{}", String::from_utf8_lossy(&link_out.stderr))))
  }

  Ok(())
}

pub fn main() {
  use std::io;
  use std::io::prelude::*;

  match main_body() {
    Ok(()) => (),
    Err(msg) => writeln!(&mut io::stderr(), "Error: {}", msg.0).unwrap(),
  }
}

impl std::error::FromError<std::io::Error> for SpiralError {
  fn from_error(err: std::io::Error) -> SpiralError {
    SpiralError(format!("{}", err))
  }
}

impl std::error::FromError<String> for SpiralError {
  fn from_error(err: String) -> SpiralError {
    SpiralError(err)
  }
}

