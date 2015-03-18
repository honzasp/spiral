#ifndef HAVE_spiral_int_hpp
#define HAVE_spiral_int_hpp
#include "spiral/val.hpp"

namespace spiral {
  extern "C" {
    auto spiral_ext_add(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_sub(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_mul(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_div(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;

    auto spiral_ext_lt(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_le(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_eq(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_ne(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_ge(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_ext_gt(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
  }
}
#endif
