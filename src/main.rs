#![allow(stable_features, deprecated)]
#![feature(collections, old_io, box_syntax)]
#![feature(core, unboxed_closures, std_misc)]
#![feature(process, old_path)]
#![allow(dead_code)]
extern crate argparse;

mod sexpr;
mod spiral;
mod spine;
mod grit;
mod asm;

#[derive(Debug, Clone)]
struct SpiralError(String);

#[derive(Debug)]
struct Args {
  source_file: String,
  output_file: Option<String>,
  output: Output,
  runtime: Option<String>,
  gas_cmd: Option<String>,
  link_cmd: Option<String>,
}

#[derive(Debug, Copy, PartialEq)]
enum Output {
  Sexpr,
  Spiral,
  Spine,
  Grit,
  Asm,
  Gas,
  Executable,
}

fn parse_args() -> Result<Args, SpiralError> {
  use argparse::{ArgumentParser, StoreConst, Store};
  let mut source_file = "".to_string();
  let mut output_file = "".to_string();
  let mut output = Output::Executable;
  let mut runtime = "".to_string();
  let mut gas_cmd = "".to_string();
  let mut link_cmd = "".to_string();

  {
    let mut ap = ArgumentParser::new();
    ap.set_description("Spiral compiler");
    ap.refer(&mut output)
      .add_option(&["--sexpr"], StoreConst(Output::Sexpr), "dump sexpr")
      .add_option(&["--spiral"], StoreConst(Output::Spiral), "dump spiral")
      .add_option(&["--spine"], StoreConst(Output::Spine), "dump spine")
      .add_option(&["--grit"], StoreConst(Output::Grit), "dump grit")
      .add_option(&["--asm"], StoreConst(Output::Asm), "dump asm")
      .add_option(&["--gas"], StoreConst(Output::Gas), "emit gas");
    ap.refer(&mut runtime)
      .add_option(&["--runtime"], Store, "path to the runtime library");
    ap.refer(&mut gas_cmd)
      .add_option(&["--gas-cmd"], Store, "assembler command");
    ap.refer(&mut link_cmd)
      .add_option(&["--link-cmd"], Store, "linker command");
    ap.refer(&mut output_file)
      .add_option(&["-o", "--output"], Store, "output file");
    ap.refer(&mut source_file)
      .add_argument("source", Store, "source file");

    match ap.parse_args() {
      Ok(()) => (),
      Err(_) => try!(Err(format!("invalid command-line args"))),
    }
  }

  Ok(Args {
    source_file: source_file,
    output_file: if output_file.is_empty() { None } else { Some(output_file) },
    output: output,
    runtime: if runtime.is_empty() { None } else { Some(runtime) },
    gas_cmd: if gas_cmd.is_empty() { None } else { Some(gas_cmd) },
    link_cmd: if link_cmd.is_empty() { None } else { Some(link_cmd) },
  })
}

fn main_body() -> Result<(), SpiralError> {
  use std::old_io as io;
  use std::process;
  use std::str;
  use std::old_io::{File};
  use std::old_path::{Path};

  let args = try!(parse_args());

  let output_file = match args.output_file {
    Some(ref path) => Path::new(path.clone()),
    None => {
      let ext = match args.output {
        Output::Sexpr => "txt",
        Output::Spiral => "txt",
        Output::Spine => "txt",
        Output::Grit => "txt",
        Output::Asm => "txt",
        Output::Gas => "s",
        Output::Executable => "",
      };
      Path::new(args.source_file.clone()).with_extension(ext)
    },
  };

  let dump = |txt: String| -> Result<(), SpiralError> {
    let mut output = try!(File::create(&output_file));
    try!(output.write_all(txt.as_bytes()));
    Ok(())
  };

  let dump_sexpr = |elem: &sexpr::Elem| -> Result<(), SpiralError> {
    let mut output = try!(File::create(&output_file));
    try!(output.write_all(sexpr::pretty_print::pretty_print_sexpr(elem).as_bytes()));
    try!(output.write_all(b"\n"));
    Ok(())
  };

  let input = try!(io::File::open(&Path::new(&args.source_file[..])).read_to_end());
  let input_str = try!(str::from_utf8(&input[..]));

  let sexpr = try!(sexpr::parse::parse_sexpr(input_str));
  if args.output == Output::Sexpr {
    return dump_sexpr(&sexpr);
  }

  let spiral = try!(sexpr::to_spiral::prog_from_sexpr(&sexpr));
  if args.output == Output::Spiral {
    return dump(format!("{:?}", sexpr))
  }

  let mut mod_loader = |name: &spiral::ModName| { panic!("load {:?}", name) };
  let spine = try!(spiral::to_spine::spine_from_spiral(&spiral, &mut mod_loader));
  if args.output == Output::Spine {
    return dump_sexpr(&spine::to_sexpr::prog_to_sexpr(&spine));
  }

  let grit = spine::to_grit::grit_from_spine(&spine);
  if args.output == Output::Grit {
    return dump_sexpr(&grit::to_sexpr::prog_to_sexpr(&grit));
  }

  let asm = grit::to_asm::asm_from_grit(&grit);
  if args.output == Output::Asm {
    return dump(format!("{:?}", asm))
  }

  let gas = asm::to_gas::gas_from_asm(&asm);
  if args.output == Output::Gas {
    return dump(gas)
  }

  let temp_dir = try!(io::TempDir::new("spiral"));
  let s_file_path = temp_dir.path().join(Path::new("assembly.s"));
  let o_file_path = temp_dir.path().join(Path::new("object.o"));

  try!(File::create(&s_file_path).write_all(gas.as_bytes()));

  let gas_cmd = args.gas_cmd.unwrap_or("as".to_string());
  let gas_out = try!(process::Command::new(&gas_cmd[..])
    .arg(&s_file_path)
    .arg("-o").arg(&o_file_path)
    .output());
  if !gas_out.status.success() {
    try!(Err(format!("Assembler failed:\n{}", String::from_utf8_lossy(&gas_out.stderr))))
  }

  let runtime_path = args.runtime.unwrap_or("runtime.a".to_string());
  let link_cmd = args.link_cmd.unwrap_or("clang".to_string());
  let link_out = try!(process::Command::new(&link_cmd[..])
    .arg(&o_file_path)
    .arg(&runtime_path[..])
    .arg("-o").arg(&output_file)
    .output());
  if !link_out.status.success() {
    try!(Err(format!("Linker failed:\n{}", String::from_utf8_lossy(&link_out.stderr))))
  }

  Ok(())
}

pub fn main() {
  use std::old_io as io;

  match main_body() {
    Ok(()) => (),
    Err(msg) => writeln!(&mut io::stderr(), "Error: {}", msg.0).unwrap(),
  }
}

impl std::error::FromError<std::old_io::IoError> for SpiralError {
  fn from_error(err: std::old_io::IoError) -> SpiralError {
    SpiralError(format!("{}", err))
  }
}

impl std::error::FromError<std::io::Error> for SpiralError {
  fn from_error(err: std::io::Error) -> SpiralError {
    SpiralError(format!("{}", err))
  }
}

impl std::error::FromError<std::str::Utf8Error> for SpiralError {
  fn from_error(err: std::str::Utf8Error) -> SpiralError {
    SpiralError(format!("{}", err))
  }
}

impl std::error::FromError<String> for SpiralError {
  fn from_error(err: String) -> SpiralError {
    SpiralError(err)
  }
}

