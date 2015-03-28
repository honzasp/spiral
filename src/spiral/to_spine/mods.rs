use std::collections::{HashSet};
use spiral;
use spiral::to_spine::{ProgSt, Env, Res};
use spiral::to_spine::decls::{translate_decls};
use spine::onion::{Onion};

pub struct LoadedMod {
  module: spiral::Mod,
  required: HashSet<spiral::ModName>,
}

pub fn load_mods(prog: &spiral::Prog,
  mod_loader: &mut (FnMut(&spiral::ModName) -> Result<spiral::Mod, String>))
  -> Res<Vec<LoadedMod>>
{
  let mut loaded_mods = Vec::new();
  let mut loaded_names = HashSet::new();
  let mut required_names = spiral::imported::collect_prog(prog);

  loop {
    let mut new_mods = Vec::new();

    for required_name in required_names.into_iter() {
      if !loaded_names.contains(&required_name) {
        let module = try!(mod_loader(&required_name));
        if module.name != required_name {
          return Err(format!("Loaded module '{}', but got '{}'",
            required_name.0, module.name.0));
        }
        loaded_names.insert(required_name);
        new_mods.push(LoadedMod {
          required: spiral::imported::collect_mod(&module),
          module: module,
        });
      }
    }

    if new_mods.is_empty() {
      break
    } else {
      required_names = HashSet::new();
      for new_mod in new_mods.into_iter() {
        required_names.extend(new_mod.required.iter().cloned());
        loaded_mods.push(new_mod);
      }
    }
  }

  Ok(loaded_mods)
}

pub fn translate_mods(st: &mut ProgSt, env: Env, loaded_mods: Vec<LoadedMod>)
  -> Res<(Vec<Onion>, Env)>
{
  let mut remaining_mods = loaded_mods;
  let mut translated_names = HashSet::new();
  let mut translated_onions = Vec::new();
  let mut mods_env = env;

  while !remaining_mods.is_empty() {
    let mut any_translated = false;
    let mut still_remaining_mods = Vec::new();
    for remaining_mod in remaining_mods.into_iter() {
      if remaining_mod.required.iter().all(|req| translated_names.contains(req)) {
        let (onion, exports) = try!(translate_decls(st, &mods_env,
          &remaining_mod.module.decls[..]));
        let mod_name = remaining_mod.module.name.clone();
        translated_names.insert(mod_name.clone());
        mods_env = mods_env.bind_mod(mod_name, exports);
        translated_onions.push(onion);
        any_translated = true;
      } else {
        still_remaining_mods.push(remaining_mod);
      }
    }

    if !any_translated {
      return Err(format!("some modules are mutually dependent"));
    } else {
      remaining_mods = still_remaining_mods;
    }
  }

  Ok((translated_onions, mods_env))
}
