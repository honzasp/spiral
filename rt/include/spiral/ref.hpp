#ifndef HAVE_spiral_ref_hpp
#define HAVE_spiral_ref_hpp
#include "spiral/core.hpp"

namespace spiral {
  struct RefObj {
    const ObjTable* otable;
    Val value;
    bool is_mutable;
  };

  auto ref_from_val(Bg* bg, Val val) -> RefObj*;
  auto ref_to_val(RefObj* obj) -> Val;
  auto ref_from_obj_ptr(void* obj_ptr) -> RefObj*;

  void ref_stringify(Bg* bg, Buffer* buf, void* obj_ptr);
  auto ref_length(void* obj_ptr) -> uint32_t;
  auto ref_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val;
  void ref_scavenge(GcCtx* gc_ctx, void* obj_ptr);
  void ref_drop(Bg* bg, void* obj_ptr);
  auto ref_eqv(Bg* bg, void* l_ptr, void* r_ptr) -> bool;
  auto ref_equal(Bg* bg, void* l_ptr, void* r_ptr) -> bool;

  extern const ObjTable ref_otable;

  extern "C" {
    auto spiral_std_ref_new(Bg* bg, void* sp, uint32_t val_) -> uint32_t;
    auto spiral_std_is_ref(Bg* bg, void* sp, uint32_t ref_) -> uint32_t;
    auto spiral_std_ref_get(Bg* bg, void* sp, uint32_t ref_) -> uint32_t;
    auto spiral_std_ref_set(Bg* bg, void* sp, uint32_t ref_, uint32_t val_) -> uint32_t;

    auto spiral_std_sym_new(Bg* bg, void* sp, uint32_t val_) -> uint32_t;
    auto spiral_std_is_sym(Bg* bg, void* sp, uint32_t sym_) -> uint32_t;
    auto spiral_std_sym_get(Bg* bg, void* sp, uint32_t sym_) -> uint32_t;
  }
}
#endif
