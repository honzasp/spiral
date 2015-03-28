#ifndef HAVE_spiral_array_hpp
#define HAVE_spiral_array_hpp
#include "spiral/core.hpp"

namespace spiral {
  struct ArrayObj {
    const ObjTable* otable;
    uint32_t length;
    uint32_t capacity;
    Val* data;
  };

  auto array_from_val(Bg* bg, Val val) -> ArrayObj*;
  auto array_to_val(ArrayObj* obj) -> Val;
  auto array_from_obj_ptr(void* obj_ptr) -> ArrayObj*;

  void array_print(Bg* bg, FILE* stream, Val val);
  auto array_length(void* obj_ptr) -> uint32_t;
  auto array_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val;
  void array_scavenge(GcCtx* gc_ctx, void* obj_ptr);
  void array_drop(Bg* bg, void* obj_ptr);

  extern const ObjTable array_otable;

  extern "C" {
    auto spiral_std_array_new(Bg* bg, void* sp) -> uint32_t; 
    auto spiral_std_is_array(Bg* bg, void* sp, uint32_t val) -> uint32_t;
    auto spiral_std_array_push(Bg* bg, void* sp, uint32_t ary, uint32_t x) -> uint32_t;
    auto spiral_std_array_pop(Bg* bg, void* sp, uint32_t ary) -> uint32_t;
    auto spiral_std_array_len(Bg* bg, void* sp, uint32_t ary) -> uint32_t; 
    auto spiral_std_array_get(Bg* bg, void* sp, uint32_t ary, uint32_t idx) -> uint32_t; 
    auto spiral_std_array_set(Bg* bg, void* sp, uint32_t ary, uint32_t idx,
        uint32_t val) -> uint32_t; 
  }
}
#endif

