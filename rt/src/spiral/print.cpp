#include "spiral/print.hpp"

namespace spiral {
  void println(FILE* stream, Val val)
  {
    print(stream, val);
    std::fprintf(stream, "\n");
  }

  void print(FILE* stream, Val val)
  {
    if(val.is_int()) {
      std::fprintf(stream, "%i", val.unwrap_int());
    } else if(val.is_fun()) {
      std::fprintf(stream, "<fun>");
    } else {
      val.get_otable()->print_fun(stream, val);
    }
  }

  extern "C" {
    auto spiral_ext_println(uint32_t val_) -> uint32_t {
      println(stdout, Val(val_));
      return Val::true_val.u32;
    }
  }
}
