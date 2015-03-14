#ifndef HAVE_spiral_operators_hpp
#define HAVE_spiral_operators_hpp
#include "spiral/defs.hpp"

namespace spiral {
  extern "C" {
    uint32_t spiral_ext_println(uint32_t x);
    auto spiral_ext_add(uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_sub(uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_mul(uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_div(uint32_t a, uint32_t b) -> uint32_t;

    auto spiral_ext_lt(uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_le(uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_eq(uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_ne(uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_ge(uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_gt(uint32_t a, uint32_t b) -> uint32_t;
  }
}
#endif
