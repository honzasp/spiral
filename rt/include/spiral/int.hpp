#ifndef HAVE_spiral_int_hpp
#define HAVE_spiral_int_hpp
#include "spiral/val.hpp"

namespace spiral {
  extern "C" {
    auto spiral_std_add(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_sub(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_mul(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_div(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;

    auto spiral_std_lt(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_le(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_eq(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_ne(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_ge(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_gt(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
  }
}
#endif
