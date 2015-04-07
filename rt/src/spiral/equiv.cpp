#include "spiral/equiv.hpp"

namespace spiral {
  extern "C" {
    auto spiral_std_eqv(Bg*, void*, uint32_t a_, uint32_t b_) -> uint32_t {
      return Val::wrap_bool(a_ == b_).u32;
    }

    auto spiral_std_equal(Bg*, void*, uint32_t a_, uint32_t b_) -> uint32_t {
      return Val::wrap_bool(a_ == b_).u32;
    }
  }
}
