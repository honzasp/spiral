use spiral;
use spine;
use spiral::to_spine::{ProgSt, Env, Res};
use spiral::to_spine::stmts::{StmtRes, translate_stmts_refs};
use spine::onion::{Onion};

pub fn translate_decls(st: &mut ProgSt, env: &Env, decls: &[spiral::Decl])
  -> Res<(Onion, Vec<(spiral::Var, spine::Val)>)>
{
  if decls.len() == 0 {
    Ok((Onion::Hole, Vec::new()))
  } else {
    let stmts: Vec<_> = decls.iter()
      .take_while(|&decl| match *decl {
        spiral::Decl::Stmt(_) => true,
        spiral::Decl::Export(_) => false,
      }).map(|decl| match *decl {
        spiral::Decl::Stmt(ref stmt) => stmt,
        _ => panic!("non-stmt decls should have been filtered"),
      }).collect();

    if !stmts.is_empty() {
      let (stmts_onion, stmt_res) = try!(translate_stmts_refs(st, env, &stmts[..]));
      let (next_onion, exports) = try!(match stmt_res {
        StmtRes::Env(ref inner_env) =>
          translate_decls(st, inner_env, &decls[stmts.len()..]),
        StmtRes::Val(_) | StmtRes::Empty =>
          translate_decls(st, env, &decls[stmts.len()..]),
      });
      Ok((stmts_onion.subst_onion(next_onion), exports))
    } else {
      let mut exports = Vec::new();
      match decls[0] {
        spiral::Decl::Export(ref vars) => 
          for var in vars.iter() {
            let val = try!(env.lookup_var(var)
              .ok_or_else(|| format!("undefined exported var '{}'", var.0)));
            exports.push((var.clone(), val.clone()));
          },
        _ => panic!("non-export decl should not get there"),
      }
      let (next_onion, next_exports) = try!(translate_decls(st, env, &decls[1..]));
      exports.extend(next_exports.into_iter());
      Ok((next_onion, exports))
    }
  }
}
