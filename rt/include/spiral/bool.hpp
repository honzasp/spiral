#ifndef HAVE_spiral_bool_hpp
#define HAVE_spiral_bool_hpp
#include "spiral/print.hpp"
#include "spiral/val.hpp"

namespace spiral {
  struct BoolObj {
    const ObjTable* otable;
  };

  void bool_print(Bg* bg, FILE* stream, Val val);
  auto bool_length(void* obj_ptr) -> uint32_t;
  auto bool_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val;
  void bool_scavenge(GcCtx* gc_ctx, void* obj_ptr);
  void bool_drop(Bg* bg, void* obj_ptr);
  auto bool_eqv(Bg* bg, void* l_ptr, void* r_ptr) -> bool;

  extern const ObjTable bool_otable;
  extern const BoolObj true_obj;
  extern const BoolObj false_obj;
  extern const Val true_val;
  extern const Val false_val;
}
#endif
