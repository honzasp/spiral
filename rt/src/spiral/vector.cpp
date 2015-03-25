#include <cstdlib>
#include "spiral/core.hpp"
#include "spiral/gc.hpp"
#include "spiral/print.hpp"
#include "spiral/vector.hpp"

namespace spiral {
  static auto vector_from_val(Bg* bg, Val val) -> VectorObj*;
  static auto vector_to_val(VectorObj* obj) -> Val;
  static auto vector_from_obj_ptr(void* obj_ptr) -> VectorObj*;

  const ObjTable vector_otable = {
    "vector",
    &vector_print,
    &vector_length,
    &vector_evacuate,
    &vector_scavenge,
    &vector_drop,
  };

  auto vector_from_val(Bg* bg, Val val) -> VectorObj* {
    if(val.is_obj() && val.get_otable() == &vector_otable) {
      return reinterpret_cast<VectorObj*>(val.unwrap_obj());
    } else {
      bg_panic(bg, "expected vector");
    }
  }

  auto vector_from_obj_ptr(void* obj_ptr) -> VectorObj* {
    assert(reinterpret_cast<uint32_t>(obj_ptr) % 4 == 0);
    assert(*reinterpret_cast<const ObjTable**>(obj_ptr) == &vector_otable);
    return reinterpret_cast<VectorObj*>(obj_ptr);
  }

  auto vector_to_val(VectorObj* obj) -> Val {
    return Val::wrap_data_obj(reinterpret_cast<uint32_t*>(obj));
  }

  void vector_print(Bg* bg, FILE* stream, Val val) {
    auto vec_obj = vector_from_val(bg, val);

    std::fprintf(stream, "[");
    for(auto i = 0; i < vec_obj->length; ++i) {
      if(i != 0) {
        std::fprintf(stream, " ");
      }
      print(bg, stream, vec_obj->data[i]);
    }
    std::fprintf(stream, "]");
  }

  auto vector_length(void* obj_ptr) -> uint32_t {
    auto vec_obj = vector_from_obj_ptr(obj_ptr);
    return sizeof(VectorObj) + 4 * vec_obj->length;
  }

  auto vector_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val {
    auto old_vec_obj = vector_from_obj_ptr(obj_ptr);
    auto new_vec_obj = static_cast<VectorObj*>(gc_get_copy_space(gc_ctx,
          sizeof(VectorObj) + 4 * old_vec_obj->length));
    new_vec_obj->otable = &vector_otable;
    new_vec_obj->length = old_vec_obj->length;
    for(int32_t i = 0; i < old_vec_obj->length; ++i) {
      new_vec_obj->data[i] = old_vec_obj->data[i];
    }

    auto new_val = vector_to_val(new_vec_obj);
    gc_write_fwd_ptr(gc_ctx, obj_ptr, new_val);
    return new_val;
  }

  void vector_scavenge(GcCtx* gc_ctx, void* obj_ptr) {
    auto vec_obj = vector_from_obj_ptr(obj_ptr);
    for(int32_t i = 0; i < vec_obj->length; ++i) {
      vec_obj->data[i] = gc_evacuate(gc_ctx, vec_obj->data[i]);
    }
  }

  void vector_drop(Bg*, void*) {
  }

  extern "C" {
    auto spiral_ext_vec_make(Bg* bg, void* sp, uint32_t len_) -> uint32_t {
      auto len_val = Val(len_);
      if(!len_val.is_int()) {
        bg_panic(bg, "vector length must be an int");
      }
      if(len_val.unwrap_int() < 0) {
        bg_panic(bg, "vector length must be non-negative");
      }

      auto len = len_val.unwrap_int();
      auto vec_obj = static_cast<VectorObj*>(bg_get_obj_space(bg, sp,
            sizeof(VectorObj) + 4 * len));
      vec_obj->otable = &vector_otable;
      vec_obj->length = len;
      for(int32_t i = 0; i < len; ++i) {
        vec_obj->data[i] = Val::false_val;
      }

      return vector_to_val(vec_obj).u32;
    }

    auto spiral_ext_vec_length(Bg* bg, void*, uint32_t vec_) -> uint32_t {
      auto vec_obj = vector_from_val(bg, Val(vec_));
      return Val::wrap_int(vec_obj->length).u32;
    }

    auto spiral_ext_vec_get(Bg* bg, void*, uint32_t vec_, uint32_t idx_) -> uint32_t {
      auto vec_obj = vector_from_val(bg, Val(vec_));
      auto idx_val = Val(idx_);
      if(!idx_val.is_int()) {
        bg_panic(bg, "only ints can index vector");
      }

      auto idx = idx_val.unwrap_int();
      if(idx >= 0 && idx < vec_obj->length) {
        return vec_obj->data[idx].u32;
      } else {
        bg_panic(bg, "get index out of bounds");
      }
    }

    auto spiral_ext_vec_set(Bg* bg, void*, uint32_t vec_, uint32_t idx_,
        uint32_t val_) -> uint32_t 
    {
      auto vec_obj = vector_from_val(bg, Val(vec_));
      auto idx_val = Val(idx_);
      if(!idx_val.is_int()) {
        bg_panic(bg, "only ints can index vector");
      }

      auto idx = idx_val.unwrap_int();
      if(idx >= 0 && idx < vec_obj->length) {
        return vec_obj->data[idx].u32 = val_;
      } else {
        bg_panic(bg, "set index out of bounds");
      }
    }
  }
}
