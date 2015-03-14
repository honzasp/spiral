#ifndef HAVE_spiral_int_hpp
#define HAVE_spiral_int_hpp
#include "spiral/obj_table.hpp"
#include "spiral/val.hpp"

namespace spiral {
  extern "C" {
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
