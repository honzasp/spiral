use docopt;

static USAGE: &'static str = "
Usage: spiral [--include <path>]... [options] [--] <input>
       spiral (--help | --version)

Options:
    -h, --help            Show this message
    -v, --version         Show the version
    -o, --output <file>   Set the output file
    -I, --include <path>  Add the path for module lookup
    -e, --emit <type>     Emit the specified output.
                          Valid values are sexpr, spiral, spine, grit, asm, gas and exec
    -t, --runtime <file>  Set the path to C runtime library
    --link-cmd <cmd>      Set the linker command
    --gas-cmd <cmd>       Set the assembler command
";

#[derive(RustcDecodable, Debug, PartialEq)]
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

pub fn parse_args(args: Vec<&str>) -> Result<Args, docopt::Error> {
  use ::VERSION;
  docopt::Docopt::new(USAGE).and_then(|d| {
    d.help(true)
      .version(Some(VERSION.to_string()))
      .argv(args.into_iter())
      .decode()
    })
}

#[cfg(test)]
mod test {
  use args::{Args, Emit, parse_args};

  fn default_args() -> Args { Args {
    arg_input: "".to_string(),
    flag_output: None,
    flag_include: Vec::new(),
    flag_emit: None,
    flag_runtime: None,
    flag_link_cmd: None,
    flag_gas_cmd: None,
  } }

  #[test]
  fn test_include_flags() {
    assert_eq!(parse_args(vec![
        "spiral", "--include", "foo", "-I", "bar", "input"
      ]).unwrap(),
      Args {
        arg_input: "input".to_string(),
        flag_include: vec!["foo".to_string(), "bar".to_string()],
        .. default_args()
      });
  }

  #[test]
  fn test_emit_flag() {
    assert_eq!(parse_args(vec![
        "spiral", "--emit", "grit", "input"
      ]).unwrap(),
      Args {
        arg_input: "input".to_string(),
        flag_emit: Some(Emit::Grit),
        .. default_args()
      });
  }
}
