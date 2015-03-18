#ifndef HAVE_spiral_print_hpp
#define HAVE_spiral_print_hpp
#include "spiral/core.hpp"
#include "spiral/val.hpp"

namespace spiral {
  void println(Bg* bg, FILE* stream, Val val);
  void print(Bg* bg, FILE* stream, Val val);

  extern "C" {
    auto spiral_ext_println(Bg* bg, void* sp, uint32_t val_) -> uint32_t;
  }
}

#endif
