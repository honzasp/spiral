#include <cstdarg>
#include "spiral/gc.hpp"
#include "spiral/print.hpp"
#include "spiral/string.hpp"

namespace spiral {
  const ObjTable str_otable = {
    "string",
    &str_print,
    &str_length,
    &str_evacuate,
    &str_scavenge,
    &str_drop,
    &str_eqv,
    &str_eqv,
  };

  auto str_from_val(Bg* bg, Val val) -> StrObj* {
    if(val.is_obj() && val.get_otable() == &str_otable) {
      return val.unwrap_obj<StrObj>();
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
    assert(otable == &str_otable);
    return reinterpret_cast<StrObj*>(obj_ptr);
  }

  void str_print(Bg* bg, FILE* stream, Val val) {
    auto obj = str_from_val(bg, val);
    std::fwrite(obj->data, obj->length, 1, stream);
  }

  auto str_length(void*) -> uint32_t {
    return sizeof(StrObj);
  }

  auto str_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val {
    if(!ptr_is_static(obj_ptr)) {
      auto old_obj = str_from_obj_ptr(obj_ptr);
      auto new_obj = static_cast<StrObj*>(gc_get_copy_space(gc_ctx, sizeof(StrObj)));
      new_obj->otable = &str_otable;
      new_obj->length = old_obj->length;
      new_obj->data = old_obj->data;
      auto new_val = str_to_val(new_obj);
      gc_write_fwd_ptr(gc_ctx, obj_ptr, new_val);
      return new_val;
    } else {
      return Val::wrap_data_obj(obj_ptr);
    }
  }

  void str_scavenge(GcCtx*, void* obj_ptr) {
    assert(!ptr_is_static(obj_ptr));
  }

  void str_drop(Bg* bg, void* obj_ptr) {
    assert(!ptr_is_static(obj_ptr));
    auto obj = str_from_obj_ptr(obj_ptr);
    bg_free_mem(bg, obj->data);
  }

  auto str_eqv(Bg*, void* l_ptr, void* r_ptr) -> bool {
    auto l_obj = str_from_obj_ptr(l_ptr);
    auto r_obj = str_from_obj_ptr(r_ptr);
    if(l_obj->length != r_obj->length) {
      return false;
    }
    for(uint32_t i = 0; i < l_obj->length; ++i) {
      if(l_obj->data[i] != r_obj->data[i]) {
        return false;
      }
    }
    return true;
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

    auto spiral_std_str_cat(Bg* bg, void* sp, uint32_t count_, ...) -> uint32_t {
      auto count_val = Val(count_);
      if(!count_val.is_int()) {
        bg_panic(bg, "str cat count must be an int");
      }
      if(count_val.unwrap_int() < 0) {
        bg_panic(bg, "str cat count must be non-negative");
      }

      auto count = static_cast<uint32_t>(count_val.unwrap_int());
      uint32_t result_length = 0;
      std::va_list args;

      va_start(args, count_);
      for(uint32_t i = 0; i < count; ++i) {
        auto str_obj = str_from_val(bg, Val(va_arg(args, uint32_t)));
        result_length += str_obj->length;
      }
      va_end(args);

      auto data = static_cast<uint8_t*>(bg_alloc_mem(bg, result_length + 1));
      uint32_t data_idx = 0;
      va_start(args, count_);
      for(uint32_t i = 0; i < count; ++i) {
        auto str_obj = str_from_val(bg, Val(va_arg(args, uint32_t)));
        for(uint32_t j = 0; j < str_obj->length; ++j) {
          data[data_idx++] = str_obj->data[j];
        }
      }
      assert(data_idx == result_length);
      data[data_idx++] = 0;

      auto new_obj = static_cast<StrObj*>(bg_get_obj_space(bg, sp, sizeof(StrObj)));
      new_obj->otable = &str_otable;
      new_obj->length = result_length;
      new_obj->data = data;
      return str_to_val(new_obj).u32;
    }
  }
}
