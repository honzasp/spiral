#include "spiral/fwd_ptr.hpp"
#include "spiral/gc.hpp"

namespace spiral {
  const ObjTable fwd_ptr_otable = {
    "forward pointer (!!!)",
    &fwd_ptr_print,
    &fwd_ptr_length,
    &fwd_ptr_evacuate,
    &fwd_ptr_scavenge,
    &fwd_ptr_drop,
  };

  static auto fwd_ptr_from_obj_ptr(void* obj_ptr) -> FwdPtrObj* {
    assert(reinterpret_cast<uint32_t>(obj_ptr) % 4 == 0);
    assert(*reinterpret_cast<const ObjTable**>(obj_ptr) == &fwd_ptr_otable);
    return static_cast<FwdPtrObj*>(obj_ptr);
  }

  void fwd_ptr_print(Bg* bg, FILE*, Val) {
    bg_panic(bg, "printing forward pointer");
  }

  auto fwd_ptr_length(void*) -> uint32_t {
    return sizeof(FwdPtrObj);
  }

  auto fwd_ptr_evacuate(GcCtx*, void* obj_ptr) -> Val {
    auto fwd_ptr = fwd_ptr_from_obj_ptr(obj_ptr);
    return fwd_ptr->fwd;
  }

  void fwd_ptr_scavenge(GcCtx*, void*) {
  }

  void fwd_ptr_drop(Bg*, void*) {
  }
}
