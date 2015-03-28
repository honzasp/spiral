#include "spiral/print.hpp"

namespace spiral {
  void println(Bg* bg, FILE* stream, Val val)
  {
    print(bg, stream, val);
    std::fprintf(stream, "\n");
  }

  void print(Bg* bg, FILE* stream, Val val)
  {
    if(val.is_int()) {
      std::fprintf(stream, "%i", val.unwrap_int());
    } else if(val.is_fun()) {
      std::fprintf(stream, "<fun>");
    } else {
      val.get_otable()->print_fun(bg, stream, val);
    }
  }

  extern "C" {
    auto spiral_std_println(Bg* bg, void*, uint32_t val_) -> uint32_t {
      println(bg, stdout, Val(val_));
      return true_val.u32;
    }
  }
}
