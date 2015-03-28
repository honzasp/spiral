#include <cstdio>
#include "spiral/core.hpp"
#include "spiral/int.hpp"

namespace spiral {
  template<typename F>
  static auto binop_int(Bg* bg, uint32_t a, uint32_t b, F op) -> uint32_t {
    auto a_val = Val(a), b_val = Val(b);
    if(a_val.is_int() && b_val.is_int()) {
      return Val::wrap_int(op(a_val.unwrap_int(), b_val.unwrap_int())).u32;
    } else {
      bg_panic(bg, "binary operation did not get ints");
    }
  }

  template<typename F>
  static auto cmp_int(Bg* bg, uint32_t a, uint32_t b, F cmp) -> uint32_t {
    auto a_val = Val(a), b_val = Val(b);
    if(a_val.is_int() && b_val.is_int()) {
      auto result = cmp(a_val.unwrap_int(), b_val.unwrap_int());
      return Val::wrap_bool(result).u32;
    } else {
      bg_panic(bg, "comparison did not get ints");
    }
  }

  extern "C" {
    auto spiral_std_add(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return binop_int(bg, a, b, [](int32_t a, int32_t b){ return a + b; });
    }
    auto spiral_std_sub(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return binop_int(bg, a, b, [](int32_t a, int32_t b){ return a - b; });
    }
    auto spiral_std_mul(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return binop_int(bg, a, b, [](int32_t a, int32_t b){ return a * b; });
    }
    auto spiral_std_div(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return binop_int(bg, a, b, [](int32_t a, int32_t b){ return a / b; });
    }

    auto spiral_std_lt(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return cmp_int(bg, a, b, [](int32_t a, int32_t b){ return a < b; });
    }
    auto spiral_std_le(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return cmp_int(bg, a, b, [](int32_t a, int32_t b){ return a <= b; });
    }
    auto spiral_std_eq(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return cmp_int(bg, a, b, [](int32_t a, int32_t b){ return a == b; });
    }
    auto spiral_std_ne(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return cmp_int(bg, a, b, [](int32_t a, int32_t b){ return a != b; });
    }
    auto spiral_std_ge(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return cmp_int(bg, a, b, [](int32_t a, int32_t b){ return a >= b; });
    }
    auto spiral_std_gt(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return cmp_int(bg, a, b, [](int32_t a, int32_t b){ return a > b; });
    }
  }
}
