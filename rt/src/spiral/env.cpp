#include <cstdlib>
#include "spiral/cons.hpp"
#include "spiral/env.hpp"
#include "spiral/stack_root.hpp"
#include "spiral/string.hpp"

namespace spiral {
  extern "C" {
    auto spiral_std_env_get_argv(Bg* bg, void* sp) -> uint32_t {
      StackRoot list(bg, false_val);
      for(int32_t rev_i = 0; rev_i < bg->argc; ++rev_i) {
        auto arg_cstr = bg->argv[bg->argc - rev_i - 1];
        auto arg_str = str_to_val(str_new_from_cstr(bg, sp, arg_cstr));
        list.set(cons_to_val(cons_new(bg, sp, arg_str, list.get())));
      }
      return list.unroot(bg).u32;
    }

    auto spiral_std_env_get_var(Bg* bg, void* sp, uint32_t var) -> uint32_t {
      auto var_str = str_from_val(bg, Val(var));
      auto var_cstr = static_cast<char*>(bg_alloc_mem(bg, var_str->length + 1));
      for(uint32_t i = 0; i < var_str->length; ++i) {
        var_cstr[i] = static_cast<char>(var_str->data[i]);
      }
      var_cstr[var_str->length] = '\0';

      auto value_cstr = std::getenv(var_cstr);
      bg_free_mem(bg, var_cstr);

      if(value_cstr != 0) {
        auto value_str = str_new_from_cstr(bg, sp, value_cstr);
        return str_to_val(value_str).u32;
      } else {
        return false_val.u32;
      }
    }
  }
}
