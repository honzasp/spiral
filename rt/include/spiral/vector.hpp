#ifndef HAVE_spiral_vector_hpp
#define HAVE_spiral_vector_hpp
#include "spiral/core.hpp"

namespace spiral {
  struct VectorObj {
    const ObjTable* otable;
    int32_t length;
    Val data[0];
  };

  void vector_print(Bg* bg, FILE* stream, Val val);
  auto vector_length(void* obj_ptr) -> uint32_t;
  auto vector_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val;
  void vector_scavenge(GcCtx* gc_ctx, void* obj_ptr);
  void vector_drop(Bg* bg, void* obj_ptr);

  extern const ObjTable vector_otable;

  extern "C" {
    auto spiral_ext_vec_make(Bg* bg, void* sp, uint32_t len) -> uint32_t; 
    auto spiral_ext_vec_length(Bg* bg, void* sp, uint32_t vec) -> uint32_t; 
    auto spiral_ext_vec_get(Bg* bg, void* sp, uint32_t vec, uint32_t idx) -> uint32_t; 
    auto spiral_ext_vec_set(Bg* bg, void* sp, uint32_t vec, uint32_t idx,
        uint32_t val) -> uint32_t; 
  }
}
#endif
