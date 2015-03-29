#include <cstdarg>
#include <cstdlib>
#include "spiral/core.hpp"
#include "spiral/gc.hpp"
#include "spiral/print.hpp"
#include "spiral/tuple.hpp"

namespace spiral {
  const ObjTable tuple_otable = {
    "tuple",
    &tuple_print,
    &tuple_length,
    &tuple_evacuate,
    &tuple_scavenge,
    &tuple_drop,
  };

  auto tuple_from_val(Bg* bg, Val val) -> TupleObj* {
    if(val.is_obj() && val.get_otable() == &tuple_otable) {
      return reinterpret_cast<TupleObj*>(val.unwrap_obj());
    } else {
      bg_panic(bg, "expected tuple");
    }
  }

  auto tuple_from_obj_ptr(void* obj_ptr) -> TupleObj* {
    assert(reinterpret_cast<uint32_t>(obj_ptr) % 4 == 0);
    assert(*reinterpret_cast<const ObjTable**>(obj_ptr) == &tuple_otable);
    return reinterpret_cast<TupleObj*>(obj_ptr);
  }

  auto tuple_to_val(TupleObj* obj) -> Val {
    return Val::wrap_data_obj(reinterpret_cast<uint32_t*>(obj));
  }

  void tuple_print(Bg* bg, FILE* stream, Val val) {
    auto tuple_obj = tuple_from_val(bg, val);

    std::fprintf(stream, "(tuple-%u", tuple_obj->length);
    for(uint32_t i = 0; i < tuple_obj->length; ++i) {
      std::fprintf(stream, " ");
      print(bg, stream, tuple_obj->data[i]);
    }
    std::fprintf(stream, ")");
  }

  auto tuple_length(void* obj_ptr) -> uint32_t {
    auto tuple_obj = tuple_from_obj_ptr(obj_ptr);
    return sizeof(TupleObj) + 4 * tuple_obj->length;
  }

  auto tuple_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val {
    auto old_obj = tuple_from_obj_ptr(obj_ptr);
    auto new_obj = static_cast<TupleObj*>(gc_get_copy_space(gc_ctx,
          sizeof(TupleObj) + 4 * old_obj->length));
    new_obj->otable = &tuple_otable;
    new_obj->length = old_obj->length;
    for(uint32_t i = 0; i < old_obj->length; ++i) {
      new_obj->data[i] = old_obj->data[i];
    }

    auto new_val = tuple_to_val(new_obj);
    gc_write_fwd_ptr(gc_ctx, obj_ptr, new_val);
    return new_val;
  }

  void tuple_scavenge(GcCtx* gc_ctx, void* obj_ptr) {
    auto tuple_obj = tuple_from_obj_ptr(obj_ptr);
    for(uint32_t i = 0; i < tuple_obj->length; ++i) {
      tuple_obj->data[i] = gc_evacuate(gc_ctx, tuple_obj->data[i]);
    }
  }

  void tuple_drop(Bg*, void*) {
  }

  extern "C" {
    auto spiral_std_tuple_new(Bg* bg, void* sp, uint32_t len_, ...) -> uint32_t {
      auto len_val = Val(len_);
      if(!len_val.is_int()) {
        bg_panic(bg, "tuple length must be an int");
      }
      if(len_val.unwrap_int() < 0) {
        bg_panic(bg, "tuple length must be non-negative");
      }

      auto len = static_cast<uint32_t>(len_val.unwrap_int());
      auto tuple_obj = static_cast<TupleObj*>(bg_get_obj_space(bg, sp,
            sizeof(TupleObj) + 4 * len));
      tuple_obj->otable = &tuple_otable;
      tuple_obj->length = len;

      std::va_list args;
      va_start(args, len_);
      for(uint32_t i = 0; i < len; ++i) {
        tuple_obj->data[i].u32 = va_arg(args, uint32_t);
      }
      va_end(args);

      return tuple_to_val(tuple_obj).u32;
    }

    auto spiral_std_is_tuple(Bg*, void*, uint32_t tuple_) -> uint32_t {
      auto val = Val(tuple_);
      return Val::wrap_bool(val.is_obj() && val.get_otable() == &tuple_otable).u32;
    }

    auto spiral_std_tuple_len(Bg* bg, void*, uint32_t tuple_) -> uint32_t {
      auto tuple_obj = tuple_from_val(bg, Val(tuple_));
      return Val::wrap_int(tuple_obj->length).u32;
    }

    auto spiral_std_tuple_get(Bg* bg, void*, uint32_t tuple_, uint32_t idx_) -> uint32_t {
      auto tuple_obj = tuple_from_val(bg, Val(tuple_));
      auto idx_val = Val(idx_);
      if(!idx_val.is_int()) {
        bg_panic(bg, "only ints can index tuple");
      }

      auto idx = static_cast<uint32_t>(idx_val.unwrap_int());
      if(idx < tuple_obj->length) {
        return tuple_obj->data[idx].u32;
      } else {
        bg_panic(bg, "tuple index out of bounds");
      }
    }
  }
}
