use sexpr;

pub fn pretty_print_sexpr(elem: &sexpr::Elem) -> String {
  let mut buffer = String::new();
  pp_elem(&mut buffer, 0, elem);
  buffer
}

fn pp_elem(buffer: &mut String, indent: usize, elem: &sexpr::Elem) {
  use std::iter;
  use std::iter::AdditiveIterator;
  if let sexpr::Elem::List(ref elems) = *elem {
    let words = elems.iter().map(count_words).sum();
    if elems.is_empty() || words <= 12 {
      pp_inline_elem(buffer, elem)
    } else {
      buffer.push('(');
      pp_elem(buffer, indent + 1, &elems[0]);
      for tail_elem in (&elems[1..]).iter() {
        buffer.push('\n');
        buffer.extend(iter::repeat(' ').take(2 + 2 * indent));
        pp_elem(buffer, indent + 1, tail_elem)
      }
      buffer.push(')');
    }
  } else {
    pp_inline_elem(buffer, elem)
  } 
}

fn pp_inline_elem(buffer: &mut String, elem: &sexpr::Elem) {
  match *elem {
    sexpr::Elem::List(ref list) => {
      buffer.push('(');
      for (i, item) in list.iter().enumerate() {
        if i != 0 {
          buffer.push(' ');
        }
        pp_inline_elem(buffer, item);
      }
      buffer.push(')');
    },
    sexpr::Elem::Identifier(ref id) => {
      assert!(!id.chars().any(|ch| ch.is_whitespace() || ch == '(' || ch == ')'),
        "identifier {:?} contains bad characters", id);
      buffer.push_str(&id[..]);
    },
    sexpr::Elem::Int(num) => buffer.push_str(format!("{}", num).as_slice()),
    sexpr::Elem::Float(num) => buffer.push_str(format!("{}", num).as_slice()),
    sexpr::Elem::String(ref txt) => buffer.push_str(format!("{:?}", txt).as_slice()),
  }
}

fn count_words(elem: &sexpr::Elem) -> usize {
  use std::iter::AdditiveIterator;
  match *elem {
    sexpr::Elem::List(ref list) => list.iter().map(count_words).sum() + 1,
    sexpr::Elem::Identifier(_) => 1,
    sexpr::Elem::Int(_) | sexpr::Elem::Float(_) => 1,
    sexpr::Elem::String(ref txt) => txt.len() / 8,
  }
}

#[cfg(test)]
mod test {
  use sexpr;

  fn check_identity(txt1: &str) -> bool {
    let sexpr = sexpr::parse::parse_sexpr(txt1).unwrap();
    let txt2 = sexpr::pretty_print::pretty_print_sexpr(&sexpr);
    txt1 == txt2
  }

  #[test]
  fn test_atoms() {
    assert!(check_identity("foo"));
    assert!(check_identity("42"));
    assert!(check_identity("12.5"));
  }

  #[test]
  fn test_short_lists() {
    assert!(check_identity("(a b c)"));
    assert!(check_identity("(1 2 3 4 5 6 7 8 9 10)"));
    assert!(check_identity("(1 (3 4) (6 7) (9 10))"));
  }

  #[test]
  fn test_long_lists() {
    assert!(check_identity(concat![
        "(the-header-is-always-inline\n",
        "  (first-child with more arguments)\n",
        "  (next (child (is (nested))))\n",
        "  (this\n",
        "    (is nested more)\n",
        "    (and more)\n",
        "    (x x x x x x x x)\n",
        "    (x x x x x x x x)\n",
        "    (x x x x x x x x)\n",
        "    (x x x x x x x x))\n",
        "  and\n",
        "  this\n",
        "  is\n",
        "  the-end)"]));
  }
}
