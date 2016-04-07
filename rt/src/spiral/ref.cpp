#include "spiral/ref.hpp"
#include "spiral/buffer.hpp"
#include "spiral/print.hpp"
#include "spiral/gc.hpp"
#include "spiral/equiv.hpp"
#include "spiral/stack_root.hpp"

namespace spiral {
  const ObjTable ref_otable = {
    "ref",
    &ref_stringify,
    &ref_length,
    &ref_evacuate,
    &ref_scavenge,
    &ref_drop,
    &ref_eqv,
    &ref_equal,
  };

  auto ref_from_val(Bg* bg, Val val) -> RefObj* {
    if(val.is_obj() && val.get_otable() == &ref_otable) {
      return val.unwrap_obj<RefObj>();
    } else {
      bg_panic(bg, "expected ref");
    }
  }

  auto ref_from_obj_ptr(void* obj_ptr) -> RefObj* {
    assert(reinterpret_cast<uint32_t>(obj_ptr) % 4 == 0);
    assert(*reinterpret_cast<const ObjTable**>(obj_ptr) == &ref_otable);
    return reinterpret_cast<RefObj*>(obj_ptr);
  }

  auto ref_to_val(RefObj* obj) -> Val {
    return Val::wrap_data_obj(obj);
  }

  void ref_stringify(Bg* bg, Buffer* buf, void* obj_ptr) {
    auto ref_obj = ref_from_obj_ptr(obj_ptr);
    buffer_push_cstr(bg, buf, "(ref ");
    stringify(bg, buf, ref_obj->value);
    buffer_push_byte(bg, buf, ')');
  }

  auto ref_length(void*) -> uint32_t {
    return sizeof(RefObj);
  }

  auto ref_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val {
    auto old_obj = ref_from_obj_ptr(obj_ptr);
    auto new_obj = static_cast<RefObj*>(gc_get_copy_space(gc_ctx, sizeof(RefObj)));
    new_obj->otable = &ref_otable;
    new_obj->value = old_obj->value;
    auto new_val = ref_to_val(new_obj);
    gc_write_fwd_ptr(gc_ctx, obj_ptr, new_val);
    return new_val;
  }

  void ref_scavenge(GcCtx* gc_ctx, void* obj_ptr) {
    auto ref_obj = ref_from_obj_ptr(obj_ptr);
    ref_obj->value = gc_evacuate(gc_ctx, ref_obj->value);
  }

  void ref_drop(Bg*, void*) {
  }

  auto ref_eqv(Bg*, void* l_ptr, void* r_ptr) -> bool {
    return l_ptr == r_ptr;
  }

  auto ref_equal(Bg* bg, void* l_ptr, void* r_ptr) -> bool {
    auto l_ref = ref_from_obj_ptr(l_ptr);
    auto r_ref = ref_from_obj_ptr(r_ptr);
    return equal(bg, l_ref->value, r_ref->value);
  }

  extern "C" {
    auto spiral_std_ref_new(Bg* bg, void* sp, uint32_t val_) -> uint32_t {
      StackRoot val_root(bg, Val(val_));
      auto ref_obj = static_cast<RefObj*>(bg_get_obj_space(bg, sp, sizeof(RefObj)));
      ref_obj->otable = &ref_otable;
      ref_obj->value = val_root.get();
      val_root.unroot(bg);
      return ref_to_val(ref_obj).u32;
    }

    auto spiral_std_is_ref(Bg*, void*, uint32_t ref_) -> uint32_t {
      auto val = Val(ref_);
      return Val::wrap_bool(val.is_obj() && val.get_otable() == &ref_otable).u32;
    }

    auto spiral_std_ref_get(Bg* bg, void*, uint32_t ref_) -> uint32_t {
      auto ref = ref_from_val(bg, Val(ref_));
      return ref->value.u32;
    }

    auto spiral_std_ref_set(Bg* bg, void*, uint32_t ref_, uint32_t val_) -> uint32_t {
      auto ref = ref_from_val(bg, Val(ref_));
      auto prev_val = ref->value;
      ref->value = Val(val_);
      return prev_val.u32;
    }
  }
}
