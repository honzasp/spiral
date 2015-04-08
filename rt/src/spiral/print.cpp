#include "spiral/print.hpp"

namespace spiral {
  void stringify_short(Bg* bg, Buffer* buf, Val val) {
    if(val.is_int()) {
      buffer_push_cstr(bg, buf, "int");
    } else {
      buffer_push_cstr(bg, buf, val.get_otable()->type_name);
    }
  }

  void stringify(Bg* bg, Buffer* buf, Val val) {
    if(val.is_int()) {
      buffer_printf(bg, buf, "%i", val.unwrap_int());
    } else {
      (*val.get_otable()->stringify_fun)(bg, buf, val.unwrap_obj<void>());
    }
  }

  extern "C" {
    auto spiral_std_println(Bg* bg, void*, uint32_t x) -> uint32_t {
      Buffer buf = buffer_new(bg);
      stringify(bg, &buf, Val(x));
      buffer_push_byte(bg, &buf, '\n');
      std::fwrite(buf.data, buf.length, 1, stdout);
      buffer_drop(bg, buf);
      return true_val.u32;
    }
  }
}
