#ifndef HAVE_spiral_defs_hpp
#define HAVE_spiral_defs_hpp
#include <cstdint>

namespace spiral {
  using std::uint32_t;
  using std::int32_t;

  struct Val {
    union {
      uint32_t u32;
      int32_t i32;
      void* ptr;
    };

    explicit Val(uint32_t u32): u32(u32) { }
    explicit Val(int32_t i32): i32(i32) { }
    explicit Val(void* ptr): ptr(ptr) { }

    auto is_int() const -> bool { return (this->i32 & 1) == 0; }
    auto is_fun() const -> bool { return (this->i32 & 0b11) == 0b01; }
    auto is_obj() const -> bool { return (this->i32 & 0b11) == 0b11; }

    auto unwrap_int() const -> int32_t { return (this->i32 >> 1); }
    static auto wrap_int(int32_t num) -> Val { return Val(num << 1); }
    static inline auto wrap_bool(bool b) -> Val;
  };

  struct Ctx {
    void* heap_ptr;
  };

  const Val true_val = Val(0xfffffffd);
  const Val false_val = Val(0xffffffff);
  inline auto Val::wrap_bool(bool b) -> Val { return b ? true_val : false_val; }

}
#endif
