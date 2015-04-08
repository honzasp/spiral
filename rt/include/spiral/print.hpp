#ifndef HAVE_spiral_print_hpp
#define HAVE_spiral_print_hpp
#include "spiral/buffer.hpp"
#include "spiral/core.hpp"
#include "spiral/val.hpp"

namespace spiral {
  void stringify_short(Bg* bg, Buffer* buf, Val val);
  void stringify(Bg* bg, Buffer* buf, Val val);

  extern "C" {
    auto spiral_std_println(Bg* bg, void* sp, uint32_t val_) -> uint32_t;
  }
}

#endif
