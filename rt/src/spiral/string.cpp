#include <cctype>
#include <cfenv>
#include <cmath>
#include <cstdarg>
#include "spiral/gc.hpp"
#include "spiral/number.hpp"
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
      Buffer buf = buffer_new(bg);
      stringify(bg, &buf, Val(x));
      return Val::wrap_data_obj(str_from_buffer(bg, sp, buf)).u32;
    }

    auto spiral_std_str_to_int(Bg* bg, void*, uint32_t str) -> uint32_t {
      auto str_obj = str_from_val(bg, Val(str));

      int32_t number = 0;
      bool negative = false;
      enum {
        BEFORE_SIGN, BEFORE_DIGITS, DIGITS, AFTER_DIGITS, ERROR 
      } state = BEFORE_SIGN;

      uint32_t i = 0;
      while(i < str_obj->length) {
        auto ch = str_obj->data[i];
        switch(state) {
          case BEFORE_SIGN:
            if(ch == '+') { ++i; state = BEFORE_DIGITS; break; }
            if(ch == '-') { ++i; negative = true; state = BEFORE_DIGITS; break; }
          case BEFORE_DIGITS:
            if(ch >= '0' && ch <= '9') { state = DIGITS; break; } 
            if(std::isspace(ch)) { ++i; break; }
            state = ERROR; break;
          case DIGITS:
            if(ch >= '0' && ch <= '9') {
              number = 10 * number + static_cast<int32_t>(ch - '0');
              ++i; break;
            }
            state = AFTER_DIGITS; break;
          case AFTER_DIGITS:
            if(std::isspace(ch)) { ++i; break; } 
            state = ERROR; break;
          case ERROR:
            return false_val.u32;
        }
      }

      if(state == DIGITS || state == AFTER_DIGITS) {
        int32_t result = negative ? -number : number;
        return Val::wrap_int(result).u32;
      } else {
        return false_val.u32;
      }
    }

    auto spiral_std_str_to_number(Bg* bg, void* sp, uint32_t str) -> uint32_t {
      auto str_obj = str_from_val(bg, Val(str));

      double whole_part = 0.0;
      double frac_part = 0.0;
      double fraction = 1.0;
      bool negative = false;
      int32_t exponent = 0;
      bool exponent_negative = false;
      enum {
        BEFORE_SIGN, BEFORE_WHOLE, WHOLE_DIGITS,
        FRAC_DIGITS, EXP_SIGN, EXP_DIGITS,
        AFTER, ERROR,
      } state = BEFORE_SIGN;

      uint32_t i = 0;
      while(i < str_obj->length) {
        auto ch = str_obj->data[i];
        switch(state) {
          case BEFORE_SIGN:
            if(ch == '+') { ++i; state = BEFORE_WHOLE; break; }
            if(ch == '-') { ++i; negative = true; state = BEFORE_WHOLE; break; }
          case BEFORE_WHOLE:
            if(ch >= '0' && ch <= '9') { state = WHOLE_DIGITS; break; }
            if(ch == '.') { ++i; state = FRAC_DIGITS; break; }
            if(std::isspace(ch)) { ++i; break; }
            state = ERROR; break;
          case WHOLE_DIGITS:
            if(ch >= '0' && ch <= '9') { 
              whole_part = 10.0 * whole_part + static_cast<double>(ch - '0');
              ++i; break;
            } 
            if(ch == '.') { ++i; state = FRAC_DIGITS; break; }
            if(ch == 'e' || ch == 'E') { ++i; state = EXP_SIGN; break; }
            state = AFTER; break;
          case FRAC_DIGITS:
            if(ch >= '0' && ch <= '9') {
              fraction = fraction * 0.1;
              frac_part = frac_part + fraction * static_cast<double>(ch - '0');
              ++i; break;
            }
            if(ch == 'e' || ch == 'E') { ++i; state = EXP_SIGN; break; }
            state = AFTER; break;
          case EXP_SIGN:
            if(ch == '+') { ++i; state = EXP_DIGITS; break; }
            if(ch == '-') { ++i; exponent_negative = true; state = EXP_DIGITS; break; }
            if(ch >= '0' && ch <= '9') { state = EXP_DIGITS; break; }
            state = ERROR; break;
          case EXP_DIGITS:
            if(ch >= '0' && ch <= '9') {
              exponent = 10 * exponent + static_cast<int32_t>(ch - '0');
              ++i; break;
            }
            state = AFTER; break;
          case AFTER:
            if(std::isspace(ch)) { ++i; break; }
            state = ERROR; break;
          case ERROR:
            return false_val.u32;
        }
      }

      if(state == WHOLE_DIGITS) {
        std::feclearexcept(FE_ALL_EXCEPT);
        int32_t int_whole = std::lrint(negative ? -whole_part : whole_part);
        if(!std::fetestexcept(FE_ALL_EXCEPT) && ((int_whole << 1) >> 1) == int_whole) {
          return Val::wrap_int(int_whole).u32;
        } else {
          return double_new(bg, sp, whole_part).u32;
        }
      } else if(state == FRAC_DIGITS) {
        double base = whole_part + frac_part;
        return double_new(bg, sp, negative ? -base : base).u32;
      } else if(state == EXP_DIGITS || state == AFTER) {
        double base = whole_part + frac_part;
        double mult = std::pow(10.0, static_cast<double>(
              exponent_negative ? -exponent : exponent));
        return double_new(bg, sp, (negative ? -base : base) * mult).u32;
      } else {
        return false_val.u32;
      }
    }
  }
}
