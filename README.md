# Spiral

This is the repository of my maturita paper "Compiler of a simple programming
language". It includes the sources of the compiler, runtime, tests and also the
paper itself.

## Abstract

Spiral is an impure untyped functional language inspired by Scheme, featuring
first-class functions, tail-calls, module system and both mutable and immutable
data structures. We first translate the source language to Spine, a
continuation-passing style functional language. Spine is then transformed into
low-level imperative language Grit. Several optimization passes operate on this
intermediate language, including global value analysis and inlining. As the last
step of the compilation pipeline, we generate assembly, translate it with an
external assembler and link the resulting object file with the runtime library.
The runtime, written in C++, defines core functions and objects and manages the
heap with a moving garbage collector. The implementation includes a basic
standard library.

## How to use

To install and try the Spiral compiler, first get the [Rust
compiler](https://www.rust-lang.org). **You will need to install the nightly
version, as the compiler requires unstable Rust features.** To build the
runtime, please install `clang` and `tup`.

Then, `cargo build` from the top directory will compile the compiler (it will be
placed in `target/debug/spiral`), `tup` in the `rt/` directory will build the
runtime library (the debug version will be placed in `rt/build/runtime_debug.a`,
the fast version in `rt/build/runtime_fast.a`). Then you can run the tests with
`./tests/run.sh`.

### Compiled programs and Valgrind

When Spiral programs pass arguments to callers, they first write the arguments
on the stack below the current stack frame and then adjust the stack pointer.
This is perfectly safe, but it makes Valgrind very unhappy, producing billions
of errors about invalid writes (when the arguments are written below the stack
pointer) and uninitialized values (when these arguments are subsequently used).
To fool valgrind, call it with `--undef-value-errors=no
--workaround-gcc296-bugs=yes`.
