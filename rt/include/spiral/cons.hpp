#ifndef HAVE_spiral_cons_hpp
#define HAVE_spiral_cons_hpp
#include "spiral/core.hpp"

namespace spiral {
  struct ConsObj {
    const ObjTable* otable;
    Val car;
    Val cdr;
  };

  auto cons_from_val(Bg* bg, Val val) -> ConsObj*;
  auto cons_to_val(ConsObj* obj) -> Val;
  auto cons_from_obj_ptr(void* obj_ptr) -> ConsObj*;

  void cons_print(Bg* bg, FILE* stream, Val val);
  auto cons_length(void* obj_ptr) -> uint32_t;
  auto cons_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val;
  void cons_scavenge(GcCtx* gc_ctx, void* obj_ptr);
  void cons_drop(Bg* bg, void* obj_ptr);
  auto cons_eqv(Bg* bg, void* l_ptr, void* r_ptr) -> bool;
  auto cons_equal(Bg* bg, void* l_ptr, void* r_ptr) -> bool;

  extern const ObjTable cons_otable;

  extern "C" {
    auto spiral_std_cons_new(Bg* bg, void* sp, uint32_t car, uint32_t cdr) -> uint32_t;
    auto spiral_std_is_cons(Bg* bg, void* sp, uint32_t val) -> uint32_t;
    auto spiral_std_car(Bg* bg, void* sp, uint32_t cons) -> uint32_t;
    auto spiral_std_cdr(Bg* bg, void* sp, uint32_t cons) -> uint32_t;
  }
}
#endif
