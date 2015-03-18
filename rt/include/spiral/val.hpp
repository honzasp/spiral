#ifndef HAVE_spiral_defs_hpp
#define HAVE_spiral_defs_hpp
#include <cassert>
#include <cstdint>

namespace spiral {
  using std::uint32_t;
  using std::int32_t;

  struct Val;
  struct ObjTable;

  struct Val {
    union {
      uint32_t u32;
      int32_t i32;
      void* ptr;
    };

    explicit Val(uint32_t u32): u32(u32) { }
    explicit Val(int32_t i32): i32(i32) { }
    explicit Val(void* ptr): ptr(ptr) { }

    inline auto is_int() const -> bool;
    inline auto is_fun() const -> bool;
    inline auto is_obj() const -> bool;

    inline auto unwrap_int() const -> int32_t;
    inline auto unwrap_obj() const -> uint32_t*;
    static inline auto wrap_int(int32_t num) -> Val;
    static inline auto wrap_bool(bool b) -> Val;
    static inline auto wrap_obj(uint32_t* ptr) -> Val;
    static inline auto wrap_obj(void* ptr) -> Val;

    inline auto get_otable() const -> const ObjTable*;
    inline auto get_type_name() const -> const char*;

    static const Val true_val;
    static const Val false_val;
  };

  inline bool operator==(const Val& a, const Val& b) { return a.u32 == b.u32; }
  inline bool operator!=(const Val& a, const Val& b) { return a.u32 == b.u32; }

  auto Val::is_int() const -> bool { return (this->i32 & 0b1) == 0b0; }
  auto Val::is_fun() const -> bool { return (this->i32 & 0b11) == 0b01; }
  auto Val::is_obj() const -> bool { return (this->i32 & 0b11) == 0b11; }

  auto Val::unwrap_int() const -> int32_t {
    assert(this->is_int());
    return this->i32 >> 1; 
  }
  auto Val::unwrap_obj() const -> uint32_t* {
    assert(this->is_obj());
    return reinterpret_cast<uint32_t*>(this->u32 - 0b11);
  }

  auto Val::wrap_int(int32_t num) -> Val {
    assert(((num << 1) >> 1) == num);
    return Val(num << 1); 
  }
  auto Val::wrap_bool(bool b) -> Val {
    return b ? Val::true_val : Val::false_val; 
  }
  auto Val::wrap_obj(uint32_t* ptr) -> Val {
    assert(ptr != 0);
    assert((reinterpret_cast<uint32_t>(ptr) & 0b11) == 0);
    return Val(reinterpret_cast<uint32_t>(ptr) + 0b11);
  }
  auto Val::wrap_obj(void* ptr) -> Val {
    return Val::wrap_obj(static_cast<uint32_t*>(ptr));
  }

  auto Val::get_otable() const -> const ObjTable* {
    assert(this->is_obj());
    auto ptr = this->unwrap_obj();
    auto otable_ptr = reinterpret_cast<const ObjTable*>(ptr[0]);
    return otable_ptr;
  }
}
#endif
