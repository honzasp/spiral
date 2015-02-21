use sexpr::{Elem};

pub fn parse_sexpr(input: &str) -> Result<Elem, String> {
  match read_element(input) {
    Ok((elem, rest)) =>
      if rest.trim_left().is_empty() { 
        Ok(elem)
      } else {
         Err(format!("{} trailing characters", rest.len()))
      },
    Err(err) => Err(err),
  }
}

fn read_element<'a>(input: &'a str) -> Result<(Elem, &'a str), String> {
  let input = input.trim_left();
  match input.trim_left().slice_shift_char() {
    Some((ch, _)) => match ch {
      '(' => read_list(input),
      ch if ch.is_digit(10) => read_number(input),
      _ => read_identifier(input),
    },
    None => Err(format!("unexpected end of input")),
  }
}

fn read_number<'a>(input: &'a str) -> Result<(Elem, &'a str), String> {
  let mut whole_part = 0f32;
  let mut frac_part = 0f32;
  let mut frac_place = 0.1f32;
  let mut had_dot = false;

  let mut input = input;
  while let Some((ch, rest)) = input.slice_shift_char() {
    match ch {
      '.' if !had_dot => had_dot = true,
      '_' => { },
      ch => match ch.to_digit(10) {
        Some(digit) if !had_dot => whole_part = 10.0 * whole_part + digit as f32,
        Some(digit) if had_dot => {
          frac_part = frac_part + (digit as f32) * frac_place;
          frac_place = frac_place / 10.0;
        },
        _ => break,
      }
    }
    input = rest;
  }

  Ok((Elem::Number(whole_part + frac_part), input))
}

fn read_identifier<'a>(input: &'a str) -> Result<(Elem, &'a str), String> {
  let mut id = String::new();
  let mut input = input;
  while let Some((ch, rest)) = input.slice_shift_char() {
    match ch {
      ch if ch.is_alphanumeric() => id.push(ch),
      ch if "~!@%^&*-_+=|:<>?/".contains_char(ch) => id.push(ch),
      _ => break,
    }
    input = rest;
  }

  if !id.is_empty() {
    Ok((Elem::Identifier(id), input))
  } else {
    Err(format!("invalid identifier"))
  }
}

fn read_list<'a>(input: &'a str) -> Result<(Elem, &'a str), String> {
  let mut input = match input.slice_shift_char() {
    Some(('(', rest)) => rest,
    _ => return Err(format!("list must start with '('")),
  };

  let mut elems = Vec::new();
  loop {
    match input.trim_left().slice_shift_char() {
      Some((')', rest)) =>
        return Ok((Elem::List(elems), rest)),
      Some(_) => match read_element(input) {
        Ok((elem, rest)) => {
          input = rest;
          elems.push(elem);
        },
        Err(msg) => return Err(msg),
      },
      None => return Err(format!("unterminated list")),
    }
  }
}

#[cfg(test)]
mod test {
  use sexpr::{Elem};
  use super::{parse_sexpr};

  fn f(x: f32) -> Elem { Elem::Number(x) }
  fn i(x: i32) -> Elem { Elem::Number(x as f32) }
  fn id(id: &str) -> Elem { Elem::Identifier(id.to_string()) }

  #[test]
  fn test_number() {
    assert_eq!(parse_sexpr("23.50"), Ok(f(23.5)));
    assert_eq!(parse_sexpr(" 23_000.50"), Ok(f(23000.5)));
    assert_eq!(parse_sexpr("0.1_25"), Ok(f(0.125)));
    assert_eq!(parse_sexpr("42"), Ok(i(42)));
  }

  #[test]
  fn test_identifier() {
    assert_eq!(parse_sexpr("hello-world"), Ok(id("hello-world")));
    assert_eq!(parse_sexpr("hello!"), Ok(id("hello!")));
    assert_eq!(parse_sexpr("+-*/"), Ok(id("+-*/")));
    assert_eq!(parse_sexpr("!@%^&="), Ok(id("!@%^&=")));
    assert_eq!(parse_sexpr("<=>"), Ok(id("<=>")));
    assert_eq!(parse_sexpr("one-2_three"), Ok(id("one-2_three")));
  }

  #[test]
  fn test_list() {
    assert_eq!(parse_sexpr("(1 2 3)"), Ok(Elem::List(vec![i(1), i(2), i(3)])));
    assert_eq!(parse_sexpr("(+ 1 2)"), Ok(Elem::List(vec![id("+"), i(1), i(2)])));
    assert_eq!(parse_sexpr("(+ (neg 2) 3)"),
      Ok(Elem::List(vec![id("+"), Elem::List(vec![id("neg"), i(2)]), i(3)])));
    assert_eq!(parse_sexpr("( ) "), Ok(Elem::List(vec![])));
  }
}
