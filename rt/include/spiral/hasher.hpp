#ifndef HAVE_spiral_hasher_hpp
#define HAVE_spiral_hasher_hpp
#include "spiral/core.hpp"
#include "spiral/sip_hash.hpp"

namespace spiral {
  struct HasherObj {
    const ObjTable* otable;
    SipHasher sip;
  };

  auto hasher_from_val(Bg* bg, Val val) -> HasherObj*;
  auto hasher_to_val(HasherObj* obj) -> Val;
  auto hasher_from_obj_ptr(void* obj_ptr) -> HasherObj*;

  void hasher_stringify(Bg* bg, Buffer* buf, void* obj_ptr);
  auto hasher_length(void* obj_ptr) -> uint32_t;
  auto hasher_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val;
  void hasher_scavenge(GcCtx* gc_ctx, void* obj_ptr);
  void hasher_drop(Bg* bg, void* obj_ptr);
  auto hasher_eqv(Bg* bg, void* l_ptr, void* r_ptr) -> bool;

  extern const ObjTable hasher_otable;

  extern "C" {
    auto spiral_std_hash_int(Bg* bg, void* sp, uint32_t a_) -> uint32_t;
    auto spiral_std_hash_str(Bg* bg, void* sp, uint32_t str_) -> uint32_t;
    auto spiral_std_hash_sym(Bg* bg, void* sp, uint32_t str_) -> uint32_t;
    auto spiral_std_hasher_new(Bg* bg, void* sp) -> uint32_t;
    auto spiral_std_hasher_new_keyed(Bg* bg, void* sp,
        uint32_t k0_, uint32_t k1_) -> uint32_t;
    auto spiral_std_hasher_push_str(Bg* bg, void* sp,
        uint32_t hasher_, uint32_t str_) -> uint32_t;
    auto spiral_std_hasher_push_int(Bg* bg, void* sp,
        uint32_t hasher_, uint32_t num_) -> uint32_t;
    auto spiral_std_hasher_push_sym(Bg* bg, void* sp,
        uint32_t hasher_, uint32_t sym_) -> uint32_t;
    auto spiral_std_hasher_finish(Bg* bg, void* sp, uint32_t hasher_) -> uint32_t;
  }
}
#endif
