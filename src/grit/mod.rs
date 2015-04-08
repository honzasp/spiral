pub use grit::syntax::*;
pub mod interf;
pub mod optimize_dead_defs;
pub mod optimize_dead_vals;
pub mod optimize_inline;
pub mod optimize_values;
pub mod slot_alloc;
pub mod syntax;
pub mod to_asm;
pub mod to_sexpr;
