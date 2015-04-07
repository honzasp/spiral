#ifndef HAVE_spiral_number_hpp
#define HAVE_spiral_number_hpp
#include "spiral/core.hpp"

namespace spiral {
  struct DoubleObj {
    const ObjTable* otable;
    double num;
  };

  auto double_from_val(Bg* bg, Val val) -> DoubleObj*;
  auto double_to_val(DoubleObj* obj) -> Val;
  auto double_from_obj_ptr(void* obj_ptr) -> DoubleObj*;
  auto double_new(Bg* bg, double num) -> Val;

  void double_print(Bg* bg, FILE* stream, Val val);
  auto double_length(void* obj_ptr) -> uint32_t;
  auto double_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val;
  void double_scavenge(GcCtx* gc_ctx, void* obj_ptr);
  void double_drop(Bg* bg, void* obj_ptr);

  extern const ObjTable double_otable;

  extern "C" {
    auto spiral_std_add(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_sub(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_mul(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_div(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_idiv(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_imod(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;

    auto spiral_std_lt(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_le(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_eq(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_ne(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_ge(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;
    auto spiral_std_gt(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t;

    auto spiral_std_floor(Bg* bg, void* sp, uint32_t a) -> uint32_t;
    auto spiral_std_ceil(Bg* bg, void* sp, uint32_t a) -> uint32_t;
  }
}
#endif
