#include "spiral/buffer.hpp"
#include "spiral/equiv.hpp"
#include "spiral/gc.hpp"
#include "spiral/hasher.hpp"
#include "spiral/number.hpp"
#include "spiral/print.hpp"
#include "spiral/ref.hpp"
#include "spiral/stack_root.hpp"
#include "spiral/string.hpp"

namespace spiral {
  const ObjTable hasher_otable = {
    "hasher",
    &hasher_stringify,
    &hasher_length,
    &hasher_evacuate,
    &hasher_scavenge,
    &hasher_drop,
    &hasher_eqv,
    &hasher_eqv,
  };

  auto hasher_from_val(Bg* bg, Val val) -> HasherObj* {
    if(val.is_obj() && val.get_otable() == &hasher_otable) {
      return val.unwrap_obj<HasherObj>();
    } else {
      bg_panic(bg, "expected hasher");
    }
  }

  auto hasher_from_obj_ptr(void* obj_ptr) -> HasherObj* {
    assert(reinterpret_cast<uint32_t>(obj_ptr) % 4 == 0);
    assert(*reinterpret_cast<const ObjTable**>(obj_ptr) == &hasher_otable);
    return reinterpret_cast<HasherObj*>(obj_ptr);
  }

  auto hasher_to_val(HasherObj* obj) -> Val {
    return Val::wrap_data_obj(obj);
  }

  void hasher_stringify(Bg* bg, Buffer* buf, void*) {
    buffer_push_cstr(bg, buf, "<hasher>");
  }

  auto hasher_length(void*) -> uint32_t {
    return sizeof(HasherObj);
  }

  auto hasher_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val {
    auto old_obj = hasher_from_obj_ptr(obj_ptr);
    auto new_obj = static_cast<HasherObj*>(gc_get_copy_space(gc_ctx, sizeof(HasherObj)));
    new_obj->otable = &hasher_otable;
    new_obj->sip = old_obj->sip;
    auto new_val = hasher_to_val(new_obj);
    gc_write_fwd_ptr(gc_ctx, obj_ptr, new_val);
    return new_val;
  }

  void hasher_scavenge(GcCtx*, void*) {
  }

  void hasher_drop(Bg*, void*) {
  }

  auto hasher_eqv(Bg*, void* l_ptr, void* r_ptr) -> bool {
    return l_ptr == r_ptr;
  }

  extern "C" {
    auto spiral_std_hash_int(Bg* bg, void*, uint32_t a_) -> uint32_t {
      auto a = int_from_val(bg, Val(a_));
      auto sip = sip_new(bg->hash_k0, bg->hash_k1);
      sip_push_u32(&sip, uint32_t(a));
      auto hash = sip_finish(&sip);
      return Val::wrap_int(int32_t(hash & 0x8fffffff)).u32;
    }

    auto spiral_std_hash_str(Bg* bg, void*, uint32_t str_) -> uint32_t {
      auto str = str_from_val(bg, Val(str_));
      auto sip = sip_new(bg->hash_k0, bg->hash_k1);
      sip_push_bytes(&sip, str->data, str->length);
      auto hash = sip_finish(&sip);
      return Val::wrap_int(int32_t(hash & 0x8fffffff)).u32;
    }

    auto spiral_std_hash_sym(Bg* bg, void*, uint32_t sym_) -> uint32_t {
      auto sym = ref_from_val(bg, Val(sym_));
      if(sym->is_mutable) {
        bg_panic(bg, "expected a symbol, got ref");
      }
      auto sip = sip_new(bg->hash_k0, bg->hash_k1);
      sip_push_u32(&sip, sym->id);
      auto hash = sip_finish(&sip);
      return Val::wrap_int(int32_t(hash & 0x8fffffff)).u32;
    }

    auto spiral_std_hasher_new(Bg* bg, void* sp) -> uint32_t {
      auto hasher_obj = static_cast<HasherObj*>(bg_get_obj_space(
            bg, sp, sizeof(HasherObj)));
      hasher_obj->otable = &hasher_otable;
      hasher_obj->sip = sip_new(bg->hash_k0, bg->hash_k1);
      return hasher_to_val(hasher_obj).u32;
    }

    auto spiral_std_hasher_new_keyed(Bg* bg, void* sp,
        uint32_t k0_, uint32_t k1_) -> uint32_t
    {
      auto k0 = int_from_val(bg, Val(k0_));
      auto k1 = int_from_val(bg, Val(k1_));
      auto hasher_obj = static_cast<HasherObj*>(bg_get_obj_space(
            bg, sp, sizeof(HasherObj)));
      hasher_obj->otable = &hasher_otable;
      hasher_obj->sip = sip_new(k0, k1);
      return hasher_to_val(hasher_obj).u32;
    }

    auto spiral_std_hasher_push_str(Bg* bg, void*,
        uint32_t hasher_, uint32_t str_) -> uint32_t
    {
      auto hasher = hasher_from_val(bg, Val(hasher_));
      auto str = str_from_val(bg, Val(str_));
      sip_push_bytes(&hasher->sip, str->data, str->length);
      return hasher_;
    }

    auto spiral_std_hasher_push_int(Bg* bg, void*,
        uint32_t hasher_, uint32_t num_) -> uint32_t
    {
      auto hasher = hasher_from_val(bg, Val(hasher_));
      auto num = int_from_val(bg, Val(num_));
      sip_push_u32(&hasher->sip, uint32_t(num));
      return hasher_;
    }

    auto spiral_std_hasher_push_sym(Bg* bg, void*,
        uint32_t hasher_, uint32_t sym_) -> uint32_t
    {
      auto hasher = hasher_from_val(bg, Val(hasher_));
      auto sym = ref_from_val(bg, Val(sym_));
      if(sym->is_mutable) {
        bg_panic(bg, "expected symbol to hash, got ref");
      }
      sip_push_u32(&hasher->sip, sym->id);
      return hasher_;
    }

    auto spiral_std_hasher_finish(Bg* bg, void*, uint32_t hasher_) -> uint32_t {
      auto hasher = hasher_from_val(bg, Val(hasher_));
      auto hash = sip_finish(&hasher->sip);
      return Val::wrap_int(int32_t(hash & 0x8fffffff)).u32;
    }
  }
}
