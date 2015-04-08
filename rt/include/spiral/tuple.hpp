#ifndef HAVE_spiral_tuple_hpp
#define HAVE_spiral_tuple_hpp
#include "spiral/core.hpp"

namespace spiral {
  struct TupleObj {
    const ObjTable* otable;
    uint32_t length;
    Val data[0];
  };

  auto tuple_from_val(Bg* bg, Val val) -> TupleObj*;
  auto tuple_to_val(TupleObj* obj) -> Val;
  auto tuple_from_obj_ptr(void* obj_ptr) -> TupleObj*;

  void tuple_stringify(Bg* bg, Buffer* buf, void* obj_ptr);
  auto tuple_length(void* obj_ptr) -> uint32_t;
  auto tuple_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val;
  void tuple_scavenge(GcCtx* gc_ctx, void* obj_ptr);
  void tuple_drop(Bg* bg, void* obj_ptr);
  auto tuple_eqv(Bg* bg, void* l_ptr, void* r_ptr) -> bool;
  auto tuple_equal(Bg* bg, void* l_ptr, void* r_ptr) -> bool;

  extern const ObjTable tuple_otable;

  extern "C" {
    auto spiral_std_tuple_new(Bg* bg, void* sp, uint32_t len, ...) -> uint32_t; 
    auto spiral_std_is_tuple(Bg* bg, void* sp, uint32_t tuple) -> uint32_t; 
    auto spiral_std_tuple_len(Bg* bg, void* sp, uint32_t tuple) -> uint32_t; 
    auto spiral_std_tuple_get(Bg* bg, void* sp, uint32_t tuple, uint32_t idx) -> uint32_t; 
  }
}
#endif
