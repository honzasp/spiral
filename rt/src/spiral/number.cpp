#include <cmath>
#include <cfenv>
#include <cstdio>
#include "spiral/core.hpp"
#include "spiral/gc.hpp"
#include "spiral/number.hpp"
#include "spiral/print.hpp"

namespace spiral {
  const ObjTable double_otable = {
    "double",
    &double_stringify,
    &double_length,
    &double_evacuate,
    &double_scavenge,
    &double_drop,
    &double_eqv,
    &double_eqv,
  };

  auto int_from_val(Bg* bg, Val val) -> int32_t {
    if(val.is_int()) {
      return val.unwrap_int();
    } else {
      bg_panic(bg, "expected int");
    }
  }

  auto double_from_val(Bg* bg, Val val) -> DoubleObj* {
    if(val.is_obj() && val.get_otable() == &double_otable) {
      return val.unwrap_obj<DoubleObj>();
    } else {
      bg_panic(bg, "expected double");
    }
  }

  auto double_from_obj_ptr(void* obj_ptr) -> DoubleObj* {
    assert(reinterpret_cast<uint32_t>(obj_ptr) % 4 == 0);
    assert(*reinterpret_cast<const ObjTable**>(obj_ptr) == &double_otable);
    return reinterpret_cast<DoubleObj*>(obj_ptr);
  }

  auto double_to_val(DoubleObj* obj) -> Val {
    return Val::wrap_data_obj(reinterpret_cast<uint32_t*>(obj));
  }

  auto double_new(Bg* bg, void* sp, double number) -> Val {
    std::feclearexcept(FE_ALL_EXCEPT);
    int32_t as_int = std::lrint(number);
    if(!std::fetestexcept(FE_ALL_EXCEPT) && ((as_int << 1) >> 1) == as_int) {
      return Val::wrap_int(as_int);
    } else {
      auto obj = static_cast<DoubleObj*>(bg_get_obj_space(bg, sp, sizeof(DoubleObj)));
      obj->otable = &double_otable;
      obj->num = number;
      return double_to_val(obj);
    }
  }

  void double_stringify(Bg* bg, Buffer* buf, void* obj_ptr) {
    auto double_obj = static_cast<DoubleObj*>(obj_ptr);
    buffer_printf(bg, buf, "%lg", double_obj->num);
  }

  auto double_length(void*) -> uint32_t {
    return sizeof(DoubleObj);
  }

  auto double_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val {
    auto old_obj = double_from_obj_ptr(obj_ptr);
    if(!ptr_is_static(old_obj)) {
      auto new_obj = static_cast<DoubleObj*>(gc_get_copy_space(gc_ctx, sizeof(DoubleObj)));
      new_obj->otable = &double_otable;
      new_obj->num = old_obj->num;
      auto new_val = double_to_val(new_obj);
      gc_write_fwd_ptr(gc_ctx, obj_ptr, new_val);
      return new_val;
    } else {
      return double_to_val(old_obj);
    }
  }

  void double_scavenge(GcCtx*, void* obj_ptr) {
    auto obj = double_from_obj_ptr(obj_ptr);
    assert(!ptr_is_static(obj));
  }

  void double_drop(Bg*, void* obj_ptr) {
    assert(!ptr_is_static(obj_ptr));
  }

  auto double_eqv(Bg*, void* l_ptr, void* r_ptr) -> bool {
    auto l_num = double_from_obj_ptr(l_ptr)->num;
    auto r_num = double_from_obj_ptr(r_ptr)->num;
    return l_num == r_num;
  }

  template<typename I, typename D>
  static auto binop_num(Bg* bg, void* sp, uint32_t a, uint32_t b, I int_op, D dbl_op) 
    -> uint32_t 
  {
    auto a_val = Val(a), b_val = Val(b);
    auto a_is_int = a_val.is_int();
    auto b_is_int = b_val.is_int();
    auto a_is_dbl = a_val.is_obj() && a_val.get_otable() == &double_otable;
    auto b_is_dbl = b_val.is_obj() && b_val.get_otable() == &double_otable;

    if(a_is_int && b_is_int) {
      return Val::wrap_int(int_op(a_val.unwrap_int(), b_val.unwrap_int())).u32;
    } else if(a_is_dbl && b_is_dbl) {
      auto a_dbl_obj = a_val.unwrap_obj<DoubleObj>();
      auto b_dbl_obj = b_val.unwrap_obj<DoubleObj>();
      return double_new(bg, sp, dbl_op(a_dbl_obj->num, b_dbl_obj->num)).u32;
    } else if(a_is_dbl && b_is_int) {
      auto a_dbl_obj = a_val.unwrap_obj<DoubleObj>();
      return double_new(bg, sp,
          dbl_op(a_dbl_obj->num, static_cast<double>(b_val.unwrap_int()))).u32;
    } else if(a_is_int && b_is_dbl) {
      auto b_dbl_obj = b_val.unwrap_obj<DoubleObj>();
      return double_new(bg, sp,
          dbl_op(static_cast<double>(a_val.unwrap_int()), b_dbl_obj->num)).u32;
    } else {
      bg_panic(bg, "binary operation did not get two numbers");
    }
  }

  template<typename D>
  static auto binop_dbl(Bg* bg, void* sp, uint32_t a, uint32_t b, D dbl_op) -> uint32_t {
    auto a_val = Val(a), b_val = Val(b);
    double a_dbl, b_dbl;

    if(a_val.is_int()) {
      a_dbl = static_cast<double>(a_val.unwrap_int());
    } else if(a_val.is_obj() && a_val.get_otable() == &double_otable) {
      auto a_dbl_obj = a_val.unwrap_obj<DoubleObj>();
      a_dbl = a_dbl_obj->num;
    } else {
      bg_panic(bg, "binary operation did not get a number");
    }

    if(b_val.is_int()) {
      b_dbl = static_cast<double>(b_val.unwrap_int());
    } else if(b_val.is_obj() && b_val.get_otable() == &double_otable) {
      auto b_dbl_obj = b_val.unwrap_obj<DoubleObj>();
      b_dbl = b_dbl_obj->num;
    } else {
      bg_panic(bg, "binary operation did not get a number");
    }

    return double_new(bg, sp, dbl_op(a_dbl, b_dbl)).u32;
  }

  template<typename I, typename D>
  static auto unop_num(Bg* bg, void* sp, uint32_t a, I int_op, D dbl_op) -> uint32_t {
    auto a_val = Val(a);
    if(a_val.is_int()) {
      return Val::wrap_int(int_op(a_val.unwrap_int())).u32;
    } else if(a_val.is_obj() && a_val.get_otable() == &double_otable) {
      auto a_dbl_obj = a_val.unwrap_obj<DoubleObj>();
      return double_new(bg, sp, dbl_op(a_dbl_obj->num)).u32;
    } else {
      bg_panic(bg, "unary operation did not get a number");
    }
  }

  template<typename D>
  static auto unop_dbl(Bg* bg, void* sp, uint32_t a, D dbl_op) -> uint32_t {
    auto a_val = Val(a);
    double a_dbl;

    if(a_val.is_int()) {
      a_dbl = static_cast<double>(a_val.unwrap_int());
    } else if(a_val.is_obj() && a_val.get_otable() == &double_otable) {
      auto a_dbl_obj = a_val.unwrap_obj<DoubleObj>();
      a_dbl = a_dbl_obj->num;
    } else {
      bg_panic(bg, "unary operation did not get a number");
    }

    return double_new(bg, sp, dbl_op(a_dbl)).u32;
  }

  template<typename I, typename D>
  static auto unop_dbl_int(Bg* bg, void*, uint32_t a, I int_op, D dbl_op) -> uint32_t {
    auto a_val = Val(a);
    int32_t result;

    if(a_val.is_int()) {
      result = int_op(a_val.is_int());
    } else if(a_val.is_obj() && a_val.get_otable() == &double_otable) {
      auto a_dbl_obj = a_val.unwrap_obj<DoubleObj>();
      result = dbl_op(a_dbl_obj->num);
    } else {
      bg_panic(bg, "unary operation did not get a number");
    }

    return Val::wrap_int(result).u32;
  }

  template<typename I>
  static auto binop_int(Bg* bg, void*, uint32_t a, uint32_t b, I int_op) -> uint32_t {
    auto a_val = Val(a), b_val = Val(b);
    if(a_val.is_int() && b_val.is_int()) {
      return Val::wrap_int(int_op(a_val.unwrap_int(), b_val.unwrap_int())).u32;
    } else {
      bg_panic(bg, "binary operation did not get ints");
    }
  }

  template<typename I, typename D>
  static auto cmp_num(Bg* bg, uint32_t a, uint32_t b, I int_cmp, D dbl_cmp) 
    -> uint32_t 
  {
    auto a_val = Val(a), b_val = Val(b);
    auto a_is_int = a_val.is_int();
    auto b_is_int = b_val.is_int();
    auto a_is_dbl = a_val.is_obj() && a_val.get_otable() == &double_otable;
    auto b_is_dbl = b_val.is_obj() && b_val.get_otable() == &double_otable;

    if(a_is_int && b_is_int) {
      return Val::wrap_bool(int_cmp(a_val.unwrap_int(), b_val.unwrap_int())).u32;
    } else if(a_is_dbl && b_is_dbl) {
      auto a_dbl_obj = a_val.unwrap_obj<DoubleObj>();
      auto b_dbl_obj = b_val.unwrap_obj<DoubleObj>();
      return Val::wrap_bool(dbl_cmp(a_dbl_obj->num, b_dbl_obj->num)).u32;
    } else if(a_is_dbl && b_is_int) {
      auto a_dbl_obj = a_val.unwrap_obj<DoubleObj>();
      return Val::wrap_bool(
          dbl_cmp(a_dbl_obj->num, static_cast<double>(b_val.unwrap_int()))).u32;
    } else if(a_is_int && b_is_dbl) {
      auto b_dbl_obj = b_val.unwrap_obj<DoubleObj>();
      return Val::wrap_bool(
          dbl_cmp(static_cast<double>(a_val.unwrap_int()), b_dbl_obj->num)).u32;
    } else {
      bg_panic(bg, "binary comparison did not get two numbers");
    }
  }

  extern "C" {
    auto spiral_std_add(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t {
      return binop_num(bg, sp, a, b,
          [](int32_t a, int32_t b){ return a + b; },
          [](double a, double b){ return a + b; });
    }
    auto spiral_std_sub(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t {
      return binop_num(bg, sp, a, b,
          [](int32_t a, int32_t b){ return a - b; },
          [](double a, double b){ return a - b; });
    }
    auto spiral_std_mul(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t {
      return binop_num(bg, sp, a, b,
          [](int32_t a, int32_t b){ return a * b; },
          [](double a, double b){ return a * b; });
    }
    auto spiral_std_div(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t {
      return binop_dbl(bg, sp, a, b,
          [](double a, double b){ return a / b; });
    }
    auto spiral_std_idiv(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t {
      return binop_int(bg, sp, a, b, [](int32_t a, int32_t b) {
          return a >= 0 ? a / b : -((b - a - 1) / b); 
        });
    }
    auto spiral_std_imod(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t {
      return binop_int(bg, sp, a, b, [](int32_t a, int32_t b) {
          return a >= 0 ? a % b : b - ((b - a - 1) % b) - 1; 
        });
    }

    auto spiral_std_lt(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return cmp_num(bg, a, b, 
          [](int32_t a, int32_t b){ return a < b; },
          [](double a, double b){ return a < b; });
    }
    auto spiral_std_le(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return cmp_num(bg, a, b, 
          [](int32_t a, int32_t b){ return a <= b; },
          [](double a, double b){ return a <= b; });
    }
    auto spiral_std_eq(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return cmp_num(bg, a, b, 
          [](int32_t a, int32_t b){ return a == b; },
          [](double a, double b){ return a == b; });
    }
    auto spiral_std_ne(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return cmp_num(bg, a, b, 
          [](int32_t a, int32_t b){ return a != b; },
          [](double a, double b){ return a != b; });
    }
    auto spiral_std_ge(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return cmp_num(bg, a, b, 
          [](int32_t a, int32_t b){ return a >= b; },
          [](double a, double b){ return a >= b; });
    }
    auto spiral_std_gt(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return cmp_num(bg, a, b, 
          [](int32_t a, int32_t b){ return a > b; },
          [](double a, double b){ return a > b; });
    }

    auto spiral_std_floor(Bg* bg, void* sp, uint32_t a) -> uint32_t {
      return unop_num(bg, sp, a,
          [](int32_t a){ return a; },
          [](double a){ return std::floor(a); });
    }
    auto spiral_std_ceil(Bg* bg, void* sp, uint32_t a) -> uint32_t {
      return unop_num(bg, sp, a,
          [](int32_t a){ return a; },
          [](double a){ return std::ceil(a); });
    }
    auto spiral_std_abs(Bg* bg, void* sp, uint32_t a) -> uint32_t {
      return unop_dbl(bg, sp, a, [](double a){ return std::abs(a); });
    }

    auto spiral_std_exp(Bg* bg, void* sp, uint32_t a) -> uint32_t {
      return unop_dbl(bg, sp, a, [](double a){ return std::exp(a); });
    }
    auto spiral_std_log(Bg* bg, void* sp, uint32_t a) -> uint32_t {
      return unop_dbl(bg, sp, a, [](double a){ return std::log(a); });
    }
    auto spiral_std_pow(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t {
      return binop_dbl(bg, sp, a, b, [](double a, double b){ return std::pow(a, b); });
    }
    auto spiral_std_sqrt(Bg* bg, void* sp, uint32_t a) -> uint32_t {
      return unop_dbl(bg, sp, a, [](double a){ return std::sqrt(a); });
    }

    auto spiral_std_sin(Bg* bg, void* sp, uint32_t a) -> uint32_t {
      return unop_dbl(bg, sp, a, [](double a){ return std::sin(a); });
    }
    auto spiral_std_cos(Bg* bg, void* sp, uint32_t a) -> uint32_t {
      return unop_dbl(bg, sp, a, [](double a){ return std::cos(a); });
    }
    auto spiral_std_tan(Bg* bg, void* sp, uint32_t a) -> uint32_t {
      return unop_dbl(bg, sp, a, [](double a){ return std::tan(a); });
    }
    auto spiral_std_asin(Bg* bg, void* sp, uint32_t a) -> uint32_t {
      return unop_dbl(bg, sp, a, [](double a){ return std::asin(a); });
    }
    auto spiral_std_acos(Bg* bg, void* sp, uint32_t a) -> uint32_t {
      return unop_dbl(bg, sp, a, [](double a){ return std::acos(a); });
    }
    auto spiral_std_atan(Bg* bg, void* sp, uint32_t a) -> uint32_t {
      return unop_dbl(bg, sp, a, [](double a){ return std::atan(a); });
    }
    auto spiral_std_atan_2(Bg* bg, void* sp, uint32_t a, uint32_t b) -> uint32_t {
      return binop_dbl(bg, sp, a, b, [](double a, double b){ return std::atan2(a, b); });
    }

    auto spiral_std_ifloor(Bg* bg, void* sp, uint32_t a) -> uint32_t {
      return unop_dbl_int(bg, sp, a, [](int32_t i) { return i; },
            [](double x) { return int32_t(std::floor(x)); });
    }
    auto spiral_std_iceil(Bg* bg, void* sp, uint32_t a) -> uint32_t {
      return unop_dbl_int(bg, sp, a, [](int32_t i) { return i; },
            [](double x) { return int32_t(std::ceil(x)); });
    }
  }
}

