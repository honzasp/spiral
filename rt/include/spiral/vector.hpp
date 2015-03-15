#ifndef HAVE_spiral_vector_hpp
#define HAVE_spiral_vector_hpp
#include "spiral/bg.hpp"
#include "spiral/obj_table.hpp"
#include "spiral/val.hpp"

namespace spiral {
  struct VectorObj {
    const ObjTable* otable;
    int32_t length;
    Val data[0];
  };

  auto vector_from_val(Bg* bg, Val val) -> VectorObj*;
  auto vector_to_val(VectorObj* obj) -> Val;
  void vector_print(Bg* bg, FILE* stream, Val val);

  const ObjTable vector_otable = {
    "vector",
    &vector_print,
  };

  extern "C" {
    auto spiral_ext_vec_make(Bg* bg, void* sp, uint32_t len) -> uint32_t; 
    auto spiral_ext_vec_length(Bg* bg, void* sp, uint32_t vec) -> uint32_t; 
    auto spiral_ext_vec_get(Bg* bg, void* sp, uint32_t vec, uint32_t idx) -> uint32_t; 
    auto spiral_ext_vec_set(Bg* bg, void* sp, uint32_t vec, uint32_t idx,
        uint32_t val) -> uint32_t; 
  }
}
#endif
