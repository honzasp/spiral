#include <cstdlib>
#include "spiral/array.hpp"
#include "spiral/core.hpp"
#include "spiral/equiv.hpp"
#include "spiral/gc.hpp"
#include "spiral/print.hpp"

namespace spiral {
  const ObjTable array_otable = {
    "array",
    &array_print,
    &array_length,
    &array_evacuate,
    &array_scavenge,
    &array_drop,
    &array_eqv,
    &array_equal,
  };

  auto array_from_val(Bg* bg, Val val) -> ArrayObj* {
    if(val.is_obj() && val.get_otable() == &array_otable) {
      return val.unwrap_obj<ArrayObj>();
    } else {
      bg_panic(bg, "expected array");
    }
  }

  auto array_from_obj_ptr(void* obj_ptr) -> ArrayObj* {
    assert(reinterpret_cast<uint32_t>(obj_ptr) % 4 == 0);
    assert(*reinterpret_cast<const ObjTable**>(obj_ptr) == &array_otable);
    return reinterpret_cast<ArrayObj*>(obj_ptr);
  }

  auto array_to_val(ArrayObj* obj) -> Val {
    return Val::wrap_data_obj(reinterpret_cast<uint32_t*>(obj));
  }

  void array_print(Bg* bg, FILE* stream, Val val) {
    auto ary_obj = array_from_val(bg, val);

    std::fprintf(stream, "[");
    for(uint32_t i = 0; i < ary_obj->length; ++i) {
      if(i != 0) {
        std::fprintf(stream, " ");
      }
      print(bg, stream, ary_obj->data[i]);
    }
    std::fprintf(stream, "]");
  }

  auto array_length(void*) -> uint32_t {
    return sizeof(ArrayObj);
  }

  auto array_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val {
    auto old_obj = array_from_obj_ptr(obj_ptr);
    auto new_obj = static_cast<ArrayObj*>(gc_get_copy_space(gc_ctx, sizeof(ArrayObj)));
    new_obj->otable = &array_otable;
    new_obj->length = old_obj->length;
    new_obj->capacity = old_obj->capacity;
    new_obj->data = old_obj->data;
    auto new_val = array_to_val(new_obj);
    gc_write_fwd_ptr(gc_ctx, obj_ptr, new_val);
    return new_val;
  }

  void array_scavenge(GcCtx* gc_ctx, void* obj_ptr) {
    auto ary_obj = array_from_obj_ptr(obj_ptr);
    for(uint32_t i = 0; i < ary_obj->length; ++i) {
      ary_obj->data[i] = gc_evacuate(gc_ctx, ary_obj->data[i]);
    }
  }

  void array_drop(Bg* bg, void* obj_ptr) {
    auto ary_obj = array_from_obj_ptr(obj_ptr);
    if(ary_obj->data != 0) {
      bg_free_mem(bg, ary_obj->data);
    }
  }

  auto array_eqv(Bg*, void* l_ptr, void* r_ptr) -> bool {
    return l_ptr == r_ptr;
  }

  auto array_equal(Bg* bg, void* l_ptr, void* r_ptr) -> bool {
    auto l_ary = static_cast<ArrayObj*>(l_ptr);
    auto r_ary = static_cast<ArrayObj*>(r_ptr);
    if(l_ary->length != r_ary->length) {
      return false;
    }
    for(uint32_t i = 0; i < l_ary->length; ++i) {
      if(!equal(bg, l_ary->data[i], r_ary->data[i])) {
        return false;
      }
    }
    return true;
  }

  extern "C" {
    auto spiral_std_array_new(Bg* bg, void* sp) -> uint32_t {
      auto ary_obj = static_cast<ArrayObj*>(bg_get_obj_space(bg, sp, sizeof(ArrayObj)));
      ary_obj->otable = &array_otable;
      ary_obj->length = 0;
      ary_obj->capacity = 0;
      ary_obj->data = 0;
      return array_to_val(ary_obj).u32;
    }

    auto spiral_std_is_array(Bg*, void*, uint32_t val_) -> uint32_t {
      auto val = Val(val_);
      return Val::wrap_bool(val.is_obj() && val.get_otable() == &array_otable).u32;
    }

    auto spiral_std_array_push(Bg* bg, void*, uint32_t ary_, uint32_t x) -> uint32_t {
      auto ary_obj = array_from_val(bg, Val(ary_));
      if(ary_obj->length + 1 >= ary_obj->capacity) {
        auto new_capacity = ary_obj->capacity > 4 ? 2 * ary_obj->capacity : 8;
        auto new_data = static_cast<Val*>(bg_alloc_mem(bg, 4 * new_capacity));
        for(uint32_t i = 0; i < ary_obj->length; ++i) {
          new_data[i] = ary_obj->data[i];
        }
        bg_free_mem(bg, ary_obj->data);
        ary_obj->data = new_data;
        ary_obj->capacity = new_capacity;
      }
      ary_obj->data[ary_obj->length++].u32 = x;
      return array_to_val(ary_obj).u32;
    }

    auto spiral_std_array_pop(Bg* bg, void*, uint32_t ary_) -> uint32_t {
      auto ary_obj = array_from_val(bg, Val(ary_));
      if(ary_obj->length > 0) {
        if(ary_obj->length * 3 < ary_obj->capacity) {
          auto new_capacity = ary_obj->capacity / 2;
          auto new_data = static_cast<Val*>(bg_alloc_mem(bg, 4 * new_capacity));
          for(uint32_t i = 0; i < ary_obj->length; ++i) {
            new_data[i] = ary_obj->data[i];
          }
          bg_free_mem(bg, ary_obj->data);
          ary_obj->data = new_data;
          ary_obj->capacity = new_capacity;
        }
        return ary_obj->data[--ary_obj->length].u32;
      } else {
        bg_panic(bg, "cannot pop from empty array");
      }
    }

    auto spiral_std_array_len(Bg* bg, void*, uint32_t ary_) -> uint32_t {
      auto ary_obj = array_from_val(bg, Val(ary_));
      return Val::wrap_int(static_cast<int32_t>(ary_obj->length)).u32;
    }

    auto spiral_std_array_get(Bg* bg, void*, uint32_t ary_, uint32_t idx_) -> uint32_t {
      auto ary_obj = array_from_val(bg, Val(ary_));
      auto idx_val = Val(idx_);
      if(!idx_val.is_int()) {
        bg_panic(bg, "only ints can index array");
      }

      auto idx = static_cast<uint32_t>(idx_val.unwrap_int());
      if(idx < ary_obj->length) {
        return ary_obj->data[idx].u32;
      } else {
        bg_panic(bg, "array get index out of bounds");
      }
    }

    auto spiral_std_array_set(Bg* bg, void*, uint32_t ary_, uint32_t idx_,
        uint32_t val_) -> uint32_t 
    {
      auto ary_obj = array_from_val(bg, Val(ary_));
      auto idx_val = Val(idx_);
      if(!idx_val.is_int()) {
        bg_panic(bg, "only ints can index array");
      }

      auto idx = static_cast<uint32_t>(idx_val.unwrap_int());
      if(idx < ary_obj->length) {
        return ary_obj->data[idx].u32 = val_;
      } else {
        bg_panic(bg, "array set index out of bounds");
      }
    }
  }
}

