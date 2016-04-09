#include <cstdarg>
#include "spiral/gc.hpp"
#include "spiral/number.hpp"
#include "spiral/number_parsers.hpp"
#include "spiral/print.hpp"
#include "spiral/string.hpp"

namespace spiral {
  const ObjTable str_otable = {
    "string",
    &str_stringify,
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
    return Val::wrap_data_obj(obj);
  }

  auto str_from_obj_ptr(void* obj_ptr) -> StrObj* {
    assert(reinterpret_cast<uint32_t>(obj_ptr) % 4 == 0);
    auto otable = *reinterpret_cast<const ObjTable**>(obj_ptr);
    assert(otable == &str_otable);
    return reinterpret_cast<StrObj*>(obj_ptr);
  }

  auto str_from_buffer(Bg* bg, void* sp, Buffer buf) -> StrObj* {
    auto str_obj = static_cast<StrObj*>(bg_get_obj_space(bg, sp, sizeof(StrObj)));
    str_obj->otable = &str_otable;
    str_obj->length = buf.length;
    str_obj->data = buf.data;
    return str_obj;
  }

  auto str_new_from_cstr(Bg* bg, void* sp, const char* cstr) -> StrObj* {
    uint32_t length = 0;
    while(cstr[length] != '\0') {
      ++length;
    }

    auto data = static_cast<uint8_t*>(bg_alloc_mem(bg, length));
    for(uint32_t i = 0; i < length; ++i) {
      data[i] = static_cast<uint8_t>(cstr[i]);
    }

    auto str_obj = static_cast<StrObj*>(bg_get_obj_space(bg, sp, sizeof(StrObj)));
    str_obj->otable = &str_otable;
    str_obj->length = length;
    str_obj->data = data;
    return str_obj;
  }

  void str_stringify(Bg* bg, Buffer* buf, void* obj_ptr) {
    auto str_obj = static_cast<StrObj*>(obj_ptr);
    buffer_push_bytes(bg, buf, str_obj->data, str_obj->length);
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
      gc_ctx->non_heap_alive_bytes += new_obj->length;
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
    auto spiral_std_is_str(Bg*, void*, uint32_t str_) -> uint32_t {
      return Val::wrap_bool(Val(str_).is_obj() && 
          Val(str_).get_otable() == &str_otable).u32;
    }

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

      auto data = static_cast<uint8_t*>(bg_alloc_mem(bg, result_length));
      uint32_t data_idx = 0;
      va_start(args, count_);
      for(uint32_t i = 0; i < count; ++i) {
        auto str_obj = str_from_val(bg, Val(va_arg(args, uint32_t)));
        for(uint32_t j = 0; j < str_obj->length; ++j) {
          data[data_idx++] = str_obj->data[j];
        }
      }
      assert(data_idx == result_length);

      auto new_obj = static_cast<StrObj*>(bg_get_obj_space(bg, sp, sizeof(StrObj)));
      new_obj->otable = &str_otable;
      new_obj->length = result_length;
      new_obj->data = data;
      return str_to_val(new_obj).u32;
    }

    auto spiral_std_stringify(Bg* bg, void* sp, uint32_t x) -> uint32_t {
      auto x_val = Val(x);
      if(x_val.is_obj() && x_val.get_otable() == &str_otable) {
        return x_val.u32;
      } else {
        Buffer buf = buffer_new(bg);
        stringify(bg, &buf, x_val);
        return Val::wrap_data_obj(str_from_buffer(bg, sp, buf)).u32;
      }
    }

    auto spiral_std_str_to_int(Bg* bg, void*, uint32_t str) -> uint32_t {
      auto str_obj = str_from_val(bg, Val(str));
      auto parser = IntParser();
      for(uint32_t i = 0; i < str_obj->length; ++i) {
        if(!parser.push(str_obj->data[i])) {
          return false_val.u32;
        }
      }

      int32_t number;
      if(parser.get(&number)) {
        return Val::wrap_int(number).u32;
      } else {
        return false_val.u32;
      }
    }

    auto spiral_std_str_to_number(Bg* bg, void* sp, uint32_t str) -> uint32_t {
      auto str_obj = str_from_val(bg, Val(str));
      auto parser = FloatParser();
      for(uint32_t i = 0; i < str_obj->length; ++i) {
        if(!parser.push(str_obj->data[i])) {
          return false_val.u32;
        }
      }

      double number;
      if(parser.get(&number)) {
        return double_new(bg, sp, number).u32;
      } else {
        return false_val.u32;
      }
    }

    auto spiral_std_str_cmp(Bg* bg, void*, uint32_t str_1, uint32_t str_2) -> uint32_t {
      auto str_1_obj = str_from_val(bg, Val(str_1));
      auto str_2_obj = str_from_val(bg, Val(str_2));

      for(uint32_t i = 0; ; ++i) {
        if(i >= str_1_obj->length && i >= str_2_obj->length) {
          return Val::wrap_int(0).u32;
        } else if(i >= str_1_obj->length) {
          return Val::wrap_int(-256).u32;
        } else if(i >= str_2_obj->length) {
          return Val::wrap_int(256).u32;
        }

        int32_t ch_1 = str_1_obj->data[i];
        int32_t ch_2 = str_2_obj->data[i];
        if(ch_1 != ch_2) {
          return Val::wrap_int(ch_1 - ch_2).u32;
        }
      }
    }
  }
}
