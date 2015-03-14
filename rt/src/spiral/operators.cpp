#include <cstdio>
#include "spiral/main.hpp"
#include "spiral/operators.hpp"

namespace spiral {
  template<typename F>
  static auto binop_int(uint32_t a, uint32_t b, F op) -> uint32_t {
    auto a_val = Val(a), b_val = Val(b);
    if(a_val.is_int() && b_val.is_int()) {
      return Val::wrap_int(op(a_val.unwrap_int(), b_val.unwrap_int())).u32;
    } else {
      panic("binary operation did not get ints");
    }
  }

  template<typename F>
  static auto cmp_int(uint32_t a, uint32_t b, F cmp) -> uint32_t {
    auto a_val = Val(a), b_val = Val(b);
    if(a_val.is_int() && b_val.is_int()) {
      auto result = cmp(a_val.unwrap_int(), b_val.unwrap_int());
      return Val::wrap_bool(result).u32;
    } else {
      panic("comparison did not get ints");
    }
  }

  extern "C" {
    auto spiral_ext_println(uint32_t x) -> uint32_t {
      auto x_val = Val(x);
      if(x_val.is_int()) {
        std::printf("%i\n", x_val.unwrap_int());
      } else {
        std::printf("%p\n", x_val.ptr);
      }
      return true_val.u32;
    }

    auto spiral_ext_add(uint32_t a, uint32_t b) -> uint32_t {
      return binop_int(a, b, [](int32_t a, int32_t b){ return a + b; });
    }
    auto spiral_ext_sub(uint32_t a, uint32_t b) -> uint32_t {
      return binop_int(a, b, [](int32_t a, int32_t b){ return a - b; });
    }
    auto spiral_ext_mul(uint32_t a, uint32_t b) -> uint32_t {
      return binop_int(a, b, [](int32_t a, int32_t b){ return a * b; });
    }
    auto spiral_ext_div(uint32_t a, uint32_t b) -> uint32_t {
      return binop_int(a, b, [](int32_t a, int32_t b){ return a / b; });
    }

    auto spiral_ext_lt(uint32_t a, uint32_t b) -> uint32_t {
      return cmp_int(a, b, [](int32_t a, int32_t b){ return a < b; });
    }
    auto spiral_ext_le(uint32_t a, uint32_t b) -> uint32_t {
      return cmp_int(a, b, [](int32_t a, int32_t b){ return a <= b; });
    }
    auto spiral_ext_eq(uint32_t a, uint32_t b) -> uint32_t {
      return cmp_int(a, b, [](int32_t a, int32_t b){ return a == b; });
    }
    auto spiral_ext_ne(uint32_t a, uint32_t b) -> uint32_t {
      return cmp_int(a, b, [](int32_t a, int32_t b){ return a != b; });
    }
    auto spiral_ext_ge(uint32_t a, uint32_t b) -> uint32_t {
      return cmp_int(a, b, [](int32_t a, int32_t b){ return a >= b; });
    }
    auto spiral_ext_gt(uint32_t a, uint32_t b) -> uint32_t {
      return cmp_int(a, b, [](int32_t a, int32_t b){ return a > b; });
    }
  }
}
