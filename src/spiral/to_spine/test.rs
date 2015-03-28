#![cfg(test)]
use sexpr;
use spine;
use spiral;
use spine::eval::{RtVal};
use spine::eval::RtVal::{Int, True, False};

fn run(txt: &str) -> Vec<RtVal> {
  let sexpr = sexpr::parse::parse_sexpr(txt).unwrap();
  let spiral = sexpr::to_spiral::prog_from_sexpr(&sexpr).unwrap();
  let mut mod_loader = |_: &spiral::ModName| panic!("mod loaded");
  let spine = spiral::to_spine::spine_from_spiral(&spiral, &mut mod_loader).unwrap();
  let errors = spine::check::check(&spine);
  if !errors.is_empty() {
    panic!("spine invalid: {:?}", errors)
  }
  let spine_sexpr = spine::to_sexpr::prog_to_sexpr(&spine);
  println!("{}", sexpr::pretty_print::pretty_print_sexpr(&spine_sexpr));
  spine::eval::eval(&spine)
}

fn run_mods(mod_txts: Vec<(&str, &str)>, prog_txt: &str) -> Vec<RtVal> {
  let mut mod_loader = |mod_name: &spiral::ModName| {
    for &(name, txt) in mod_txts.iter() {
      if name == &mod_name.0[..] {
        let mod_sexpr = try!(sexpr::parse::parse_sexpr(txt));
        return sexpr::to_spiral::mod_from_sexpr(&mod_sexpr)
      }
    }
    Err(format!("mod not found"))
  };

  let prog_sexpr = sexpr::parse::parse_sexpr(prog_txt).unwrap();
  let prog_spiral = sexpr::to_spiral::prog_from_sexpr(&prog_sexpr).unwrap();
  let spine = spiral::to_spine::spine_from_spiral(&prog_spiral, &mut mod_loader).unwrap();

  let errors = spine::check::check(&spine);
  if !errors.is_empty() {
    panic!("spine invalid: {:?}", errors);
  }
  let spine_sexpr = spine::to_sexpr::prog_to_sexpr(&spine);
  println!("{}", sexpr::pretty_print::pretty_print_sexpr(&spine_sexpr));
  spine::eval::eval(&spine)
}

#[test]
fn test_empty_program() {
  assert_eq!(run("(program)"), vec![]);
}

#[test]
fn test_output() {
  assert_eq!(run("(program (println 1) (println 2))"), vec![Int(1), Int(2)]);
}

#[test]
fn test_shadowed_vars() {
  assert_eq!(run("(program (var x 10) (var x 20) (println x))"),
    vec![Int(20)]);
  assert_eq!(run("(program (var f +) (fun f (a b) (* a b)) (println (f 2 3)))"),
    vec![Int(6)]);
}


#[test]
fn test_one_mod() {
  assert_eq!(run_mods(vec![
    ("math",
      "(module math
        (fun inc (x) (+ x 1))
        (var pi 3)
        (fun dec (x) (- x 1))
        (export inc pi dec))"),
    ], "(program
      (import math)
      (println (inc 9))
      (println (dec pi)))"),
    vec![Int(10), Int(2)]);
}

#[test]
fn test_one_mod_only_funs() {
  assert_eq!(run_mods(vec![
    ("math",
      "(module math
        (fun double (x) (+ x x))
        (fun square (x) (* x x))
        (export double square))"),
    ], "(program
      (import math)
      (println (double 6)))"),
    vec![Int(12)]);
}


#[test]
fn test_multiple_mods() {
  assert_eq!(run_mods(vec![
      ("numbers", "(module numbers
        (var zero 0)
        (var one 1)
        (var three 3)
        (export zero one three))"),
      ("math", "(module math
        (fun double (x) (+ x x))
        (fun square (x) (* x x))
        (export double square))"),
      ("big-numbers", "(module big-numbers
        (import numbers math)
        (var nine (square three))
        (var two (double one))
        (export two nine))"),
    ], "(program
      (import big-numbers)
      (println nine)
      (println two))"),
    vec![Int(9), Int(2)]);
}

#[test]
fn test_filtered_imports() {
  let mods = vec![
    ("math", "(module math
      (fun log (z) 1)
      (fun cos (y) (- 1 y))
      (fun sin (x) x)
      (export log cos sin))"),
    ("non-math", "(module non-math
      (fun log (x) (println x))
      (fun sin (x) 666)
      (export log sin))"),
  ];

  assert_eq!(run_mods(mods.clone(), "(program
      (import non-math)
      (import (only math sin))
      (log (sin 42)))"),
    vec![Int(42)]);
  assert_eq!(run_mods(mods.clone(), "(program
      (import non-math)
      (import (except math log))
      (log (sin 30)))"),
    vec![Int(30)]);
  assert_eq!(run_mods(mods, "(program
      (import (prefix non-math nm.))
      (import (prefix math m.))
      (nm.log (m.sin 19)))"),
    vec![Int(19)]);
}

#[test]
fn test_shadowed_imports() {
  assert_eq!(run_mods(vec![
    ("mod-xyz", "(module mod-xyz
      (var x 10)
      (var y 20)
      (var z 30)
      (export x y z))"),
    ("mod-wxy", "(module mod-wxy
      (var w 1)
      (var x 2)
      (var y 3)
      (export w x y))"),
    ], "(program
      (import mod-xyz)
      (import mod-wxy)
      (println x)
      (println y)
      (println z)
      (println w))"),
    vec![Int(2), Int(3), Int(30), Int(1)]);
}

#[test]
fn test_if() {
  assert_eq!(run(
    "(program 
      (println (if false 10 20))
      (println (if true 10 20))
      (println (if 4 10 20)))"),
    vec![Int(20), Int(10), Int(10)]);
}

#[test]
fn test_cond() {
  assert_eq!(run("(program (println (cond ((== 1 2) 99) ((< 1 2) 42) (1 66))))"),
    vec![Int(42)]);
  assert_eq!(run("(program (println (cond ((== 1 2) 99) ((> 1 2) 33) (true 42))))"),
    vec![Int(42)]);
}

#[test]
fn test_when() {
  assert_eq!(run("(program (when (== 2 2) (println 4)) (when (== 1 2) (println 3)))"),
    vec![Int(4)]);
  assert_eq!(run("(program (when (== 4 4) (println 1) (println 2)) (println 4))"),
    vec![Int(1), Int(2), Int(4)]);
  assert_eq!(run("(program (when (< 4 4) (println 1) (println 2)) (println 4))"),
    vec![Int(4)]);
}

#[test]
fn test_unless() {
  assert_eq!(run("(program (unless (== 2 2) (println 4)) (unless (== 3 2) (println 3)))"),
    vec![Int(3)]);
  assert_eq!(run("(program (unless (== 4 4) (println 1) (println 2)) (println 4))"),
    vec![Int(4)]);
  assert_eq!(run("(program (unless (< 4 4) (println 1) (println 2)) (println 4))"),
    vec![Int(1), Int(2), Int(4)]);
}

#[test]
fn test_do() {
  assert_eq!(run(
    "(program (do ((f1 0 f2) (f2 1 (+ f1 f2)) (i 0 (+ i 1)))
                  ((>= i 10))
                (println f1)))"),
    vec![Int(0), Int(1), Int(1), Int(2), Int(3),
      Int(5), Int(8), Int(13), Int(21), Int(34)]);
}

#[test]
fn test_and() {
  assert_eq!(run(
      "(program
        (println (and 1 true 2))
        (println (and 3 4 false 5))
        (println (and)))"),
      vec![Int(2), False, True]);
}

#[test]
fn test_or() {
  assert_eq!(run(
      "(program
        (println (or false 1 2))
        (println (or false false))
        (println (or)))"),
      vec![Int(1), False, False]);
}

#[test]
fn test_begin() {
  assert_eq!(run("(program 
      (println 2)
      (begin
        (var x 1)
        (var y 3)
        (println (+ x y)))
      (println 8))"),
    vec![Int(2), Int(4), Int(8)]);
}

#[test]
fn test_let() {
  assert_eq!(run("(program
      (println
        (let ((x 1) (y (+ x 1)) (z (+ y 1)) (w (+ z 1)))
            w)))"),
    vec![Int(4)]);
}

#[test]
fn test_call() {
  assert_eq!(run("(program
      (fun square (x) (* x x))
      (fun neg (x) (- 0 x))
      (println (neg (square 2))))"),
    vec![Int(-4)]);
}

#[test]
fn test_recursive_fun() {
  assert_eq!(run("(program
      (fun fac (x)
        (if (<= x 1) 1
          (* x (fac (- x 1)))))
      (println (fac 6))) "),
    vec![Int(720)]);
}

#[test]
fn test_mutually_recursive_funs() {
  assert_eq!(run("(program
      (fun is-odd (n)
        (if (== n 0) false (is-even (- n 1))))
      (fun is-even (n)
        (if (== n 0) true (is-odd (- n 1))))
      (println (is-odd 6))
      (println (is-even 8))
      (println (is-odd 9)))"),
    vec![False, True, True]);
}

#[test]
fn test_var_stmt() {
  assert_eq!(run("(program
    (fun return-42 () 42)
    (var forty-two (return-42))
    (println forty-two))"), vec![Int(42)]);
}

#[test]
fn test_mutually_recursive_fun_in_var_stmt() {
  assert_eq!(run("(program
      (fun f-1 (x)
        (when (> x 1)
          (var y (f-2 (- x 1)))
          (println y)))
      (fun f-2 (x)
        (f-1 (- x 1))
        x)
      (f-1 4))"), vec![Int(1), Int(3)]);
}
