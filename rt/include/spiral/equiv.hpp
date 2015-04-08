#ifndef HAVE_spiral_equiv_hpp
#define HAVE_spiral_equiv_hpp
#include "spiral/core.hpp"

namespace spiral {
  auto eqv(Bg* bg, Val l, Val r) -> bool;
  auto equal(Bg* bg, Val l, Val r) -> bool;

  extern "C" {
    auto spiral_std_eqv(Bg* bg, void* sp, uint32_t a_, uint32_t b_) -> uint32_t;
    auto spiral_std_equal(Bg* bg, void* sp, uint32_t a_, uint32_t b_) -> uint32_t;
  }
}

#endif
