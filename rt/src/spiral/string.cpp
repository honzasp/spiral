#include "spiral/gc.hpp"
#include "spiral/print.hpp"
#include "spiral/string.hpp"

namespace spiral {
  const ObjTable dyn_str_otable = {
    "string",
    &str_print,
    &str_length,
    &dyn_str_evacuate,
    &dyn_str_scavenge,
    &dyn_str_drop,
  };

  const ObjTable static_str_otable = {
    "string",
    &str_print,
    &str_length,
    &static_str_evacuate,
    &static_str_scavenge,
    &static_str_drop,
  };


  auto str_from_val(Bg* bg, Val val) -> StrObj* {
    if(val.is_obj() && (val.get_otable() == &dyn_str_otable 
          || val.get_otable() == &static_str_otable))
    {
      return reinterpret_cast<StrObj*>(val.unwrap_obj());
    } else {
      bg_panic(bg, "expected string");
    }
  }

  auto str_to_val(StrObj* obj) -> Val {
    return Val::wrap_data_obj(reinterpret_cast<uint32_t*>(obj));
  }

  auto str_from_obj_ptr(void* obj_ptr) -> StrObj* {
    assert(reinterpret_cast<uint32_t>(obj_ptr) % 4 == 0);
    auto otable = *reinterpret_cast<const ObjTable**>(obj_ptr);
    assert(otable == &dyn_str_otable || otable == &static_str_otable);
    return reinterpret_cast<StrObj*>(obj_ptr);
  }

  auto dyn_str_from_obj_ptr(void* obj_ptr) -> StrObj* {
    assert(reinterpret_cast<uint32_t>(obj_ptr) % 4 == 0);
    assert(*reinterpret_cast<const ObjTable**>(obj_ptr) == &dyn_str_otable);
    return reinterpret_cast<StrObj*>(obj_ptr);
  }

  void str_print(Bg* bg, FILE* stream, Val val) {
    auto obj = str_from_val(bg, val);
    std::fwrite(obj->data, obj->length, 1, stream);
  }

  auto str_length(void*) -> uint32_t {
    return sizeof(StrObj);
  }

  auto dyn_str_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val {
    auto old_obj = dyn_str_from_obj_ptr(obj_ptr);
    auto new_obj = static_cast<StrObj*>(gc_get_copy_space(gc_ctx, sizeof(StrObj)));
    new_obj->otable = &dyn_str_otable;
    new_obj->length = old_obj->length;
    new_obj->data = old_obj->data;
    auto new_val = str_to_val(new_obj);
    gc_write_fwd_ptr(gc_ctx, obj_ptr, new_val);
    return new_val;
  }

  void dyn_str_scavenge(GcCtx*, void*) {
  }

  void dyn_str_drop(Bg* bg, void* obj_ptr) {
    auto obj = dyn_str_from_obj_ptr(obj_ptr);
    bg_free_mem(bg, obj->data);
  }

  auto static_str_evacuate(GcCtx*, void* obj_ptr) -> Val {
    auto obj = str_from_obj_ptr(obj_ptr);
    assert(obj->otable == &static_str_otable);
    return str_to_val(obj);
  }

  void static_str_scavenge(GcCtx*, void*) {
    assert("static str scavenged");
  }

  void static_str_drop(Bg*, void*) {
    assert("static str dropped");
  }

  extern "C" {
    auto spiral_std_str_len(Bg* bg, void*, uint32_t str_) -> uint32_t {
      auto obj = str_from_val(bg, Val(str_));
      return Val::wrap_int(static_cast<int32_t>(obj->length)).u32;
    }

    auto spiral_std_str_get(Bg* bg, void*, uint32_t str_, uint32_t idx_) -> uint32_t {
      auto obj = str_from_val(bg, Val(str_));
      auto idx_val = Val(idx_);
      if(!idx_val.is_int()) {
        bg_panic(bg, "only ints can index string");
      }

      auto idx = static_cast<uint32_t>(idx_val.unwrap_int());
      if(idx < obj->length) {
        return Val::wrap_int(obj->data[idx]).u32;
      } else {
        bg_panic(bg, "string index out of bounds");
      }
    }
  }
}
