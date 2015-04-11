#include <cstdlib>
#include "spiral/buffer.hpp"
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
      auto var_buf = buffer_new(bg);
      buffer_push_bytes(bg, &var_buf, var_str->data, var_str->length);
      buffer_push_byte(bg, &var_buf, '\0');

      auto value_cstr = std::getenv(reinterpret_cast<const char*>(var_buf.data));
      buffer_drop(bg, var_buf);

      if(value_cstr != 0) {
        auto value_str = str_new_from_cstr(bg, sp, value_cstr);
        return str_to_val(value_str).u32;
      } else {
        return false_val.u32;
      }
    }
  }
}
