static USAGE: &'static str = "
Usage: spiral [options] [--] <input>
       spiral (--help | --version)

Options:
    -h, --help            Show this message
    --version             Show the version
    -o, --output <file>   Set the output file
    -I, --include <path>  Add the path for module lookup
    --emit <type>         Emit the specified output.
                          Valid values are sexpr, spiral, spine, grit, asm, gas and exec
    --runtime <file>      Set the path to C runtime library
    --link-cmd <cmd>      Set the linker command
    --gas-cmd <cmd>       Set the assembler command
";

#[derive(RustcDecodable, Debug)]
pub struct Args {
  pub arg_input: String,
  pub flag_output: Option<String>,
  pub flag_include: Vec<String>,
  pub flag_emit: Option<Emit>,
  pub flag_runtime: Option<String>,
  pub flag_link_cmd: Option<String>,
  pub flag_gas_cmd: Option<String>,
}

#[derive(RustcDecodable, Debug, Copy, PartialEq)]
pub enum Emit {
  Sexpr,
  Spiral,
  Spine,
  Grit,
  Asm,
  Gas,
  Exec,
}

pub fn parse_args() -> Args {
  use docopt::Docopt;
  Docopt::new(USAGE)
    .and_then(|d| d.decode())
    .unwrap_or_else(|e| e.exit())
}
