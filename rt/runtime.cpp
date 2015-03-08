#include <cstdio>
#include <cstdint>
#include <cstdlib>

namespace spiral {
  using u32 = std::uint32_t;
  using i32 = std::int32_t;

  static const i32 true_val = 0b011;
  static const i32 false_val = 0b101;

  auto panic(const char* msg) -> i32 {
    std::fprintf(stderr, "Panic: %s\n", msg);
    std::abort();
  }

  template<typename F>
  auto binop_int(i32 a, i32 b, F op) -> i32 {
    if((a & 1) == 0 && (b & 1) == 0) {
      return op(a >> 1, b >> 1) << 1;
    } else {
      return panic("binary operation did not get ints");
    }
  }

  template<typename F>
  auto cmp_int(i32 a, i32 b, F cmp) -> i32 {
    if((a & 1) == 0 && (b & 1) == 0) {
      return cmp(a >> 1, b >> 1) ? true_val : false_val;
    } else {
      return panic("comparison did not get ints");
    }
  }

  extern "C" {
    extern auto spiral_start() -> i32;

    auto spiral_ext_println(i32 x) -> i32 {
      if((x & 1) == 0) {
        std::printf("%i\n", x >> 1);
      } else {
        std::printf("%p\n", (void*)x);
      }
      return 0;
    }

    auto spiral_ext_add(i32 a, i32 b) -> i32 {
      return binop_int(a, b, [](i32 a, i32 b){ return a + b; });
    }
    auto spiral_ext_sub(i32 a, i32 b) -> i32 {
      return binop_int(a, b, [](i32 a, i32 b){ return a - b; });
    }
    auto spiral_ext_mul(i32 a, i32 b) -> i32 {
      return binop_int(a, b, [](i32 a, i32 b){ return a * b; });
    }
    auto spiral_ext_div(i32 a, i32 b) -> i32 {
      return binop_int(a, b, [](i32 a, i32 b){ return a / b; });
    }

    auto spiral_ext_lt(i32 a, i32 b) -> i32 {
      return cmp_int(a, b, [](i32 a, i32 b){ return a < b; });
    }
    auto spiral_ext_le(i32 a, i32 b) -> i32 {
      return cmp_int(a, b, [](i32 a, i32 b){ return a <= b; });
    }
    auto spiral_ext_eq(i32 a, i32 b) -> i32 {
      return cmp_int(a, b, [](i32 a, i32 b){ return a == b; });
    }
    auto spiral_ext_ne(i32 a, i32 b) -> i32 {
      return cmp_int(a, b, [](i32 a, i32 b){ return a != b; });
    }
    auto spiral_ext_ge(i32 a, i32 b) -> i32 {
      return cmp_int(a, b, [](i32 a, i32 b){ return a >= b; });
    }
    auto spiral_ext_gt(i32 a, i32 b) -> i32 {
      return cmp_int(a, b, [](i32 a, i32 b){ return a >= b; });
    }

    int main(int argc, char** argv)
    {
      spiral_start();
      return 0;
    }
  }
}
