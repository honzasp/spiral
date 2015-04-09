#ifndef HAVE_spiral_string_hpp
#define HAVE_spiral_string_hpp
#include "spiral/core.hpp"

namespace spiral {
  struct StrObj {
    const ObjTable* otable;
    uint32_t length;
    uint8_t* data;
  };

  auto str_from_val(Bg* bg, Val val) -> StrObj*;
  auto str_to_val(StrObj* obj) -> Val;
  auto str_from_obj_ptr(void* obj_ptr) -> StrObj*;
  auto str_from_buffer(Bg* bg, void* sp, Buffer buf) -> StrObj*;

  void str_stringify(Bg* bg, Buffer* buf, void* obj_ptr);
  auto str_length(void* obj_ptr) -> uint32_t;
  auto str_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val;
  void str_scavenge(GcCtx* gc_ctx, void* obj_ptr);
  void str_drop(Bg* bg, void* obj_ptr);
  auto str_eqv(Bg* bg, void* l_ptr, void* r_ptr) -> bool;

  extern const ObjTable str_otable;

  extern "C" {
    auto spiral_std_str_len(Bg* bg, void* sp, uint32_t str) -> uint32_t;
    auto spiral_std_str_get(Bg* bg, void* sp, uint32_t str, uint32_t idx) -> uint32_t;
    auto spiral_std_str_cat(Bg* bg, void* sp, uint32_t count, ...) -> uint32_t;
    auto spiral_std_stringify(Bg* bg, void* sp, uint32_t x) -> uint32_t;
    auto spiral_std_str_to_int(Bg* bg, void* sp, uint32_t str) -> uint32_t;
    auto spiral_std_str_to_number(Bg* bg, void* sp, uint32_t str) -> uint32_t;
  }
}
#endif
