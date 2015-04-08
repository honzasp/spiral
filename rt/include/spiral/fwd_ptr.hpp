#ifndef HAVE_spiral_fwd_ptr_hpp
#define HAVE_spiral_fwd_ptr_hpp
#include "spiral/core.hpp"

namespace spiral {
  struct FwdPtrObj {
    const ObjTable* otable;
    Val fwd;
  };

  void fwd_ptr_stringify(Bg* bg, Buffer* buf, void* obj_ptr);
  auto fwd_ptr_length(void* obj_ptr) -> uint32_t;
  auto fwd_ptr_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val;
  void fwd_ptr_scavenge(GcCtx* gc_ctx, void* obj_ptr);
  void fwd_ptr_drop(Bg* bg, void* obj_ptr);
  auto fwd_ptr_eqv(Bg* bg, void* l_ptr, void* r_ptr) -> bool;

  extern const ObjTable fwd_ptr_otable;
}
#endif
