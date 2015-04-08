#ifndef HAVE_spiral_defs_hpp
#define HAVE_spiral_defs_hpp
#include <cassert>
#include <cstdint>
#include "spiral/asm_interface.hpp"

namespace spiral {
  using std::uint32_t;
  using std::int32_t;
  using std::uint8_t;
  using std::int8_t;

  struct Val;
  struct ObjTable;
  extern const Val true_val;
  extern const Val false_val;

  struct Val {
    union {
      uint32_t u32;
      int32_t i32;
    };

    explicit Val(uint32_t u32): u32(u32) { }
    explicit Val(int32_t i32): i32(i32) { }

    inline auto is_int() const -> bool;
    inline auto is_obj() const -> bool;
    inline auto is_fun() const -> bool;
    inline auto is_data_obj() const -> bool;

    inline auto unwrap_int() const -> int32_t;
    template<typename T>
    inline auto unwrap_obj() const -> T*;

    static inline auto wrap_int(int32_t num) -> Val;
    static inline auto wrap_bool(bool b) -> Val;
    static inline auto wrap_fun(void* ptr) -> Val;
    template<typename T>
    static inline auto wrap_data_obj(T* ptr) -> Val;

    inline auto get_otable() const -> const ObjTable*;
    inline auto get_type_name() const -> const char*;
  };

  inline bool operator==(const Val& a, const Val& b) { return a.u32 == b.u32; }
  inline bool operator!=(const Val& a, const Val& b) { return a.u32 != b.u32; }

  template<typename T>
  auto ptr_is_static(const T* ptr) -> bool {
    auto ptr_int = reinterpret_cast<uint32_t>(ptr);
    return ptr_int >= spiral_static_begin && ptr_int < spiral_static_end;
  }

  auto Val::is_int() const -> bool { return (this->i32 & 0b1) == 0b0; }
  auto Val::is_obj() const -> bool { return (this->i32 & 0b1) == 0b1; }
  auto Val::is_fun() const -> bool { return (this->i32 & 0b11) == 0b01; }
  auto Val::is_data_obj() const -> bool { return (this->i32 & 0b11) == 0b11; }

  auto Val::unwrap_int() const -> int32_t {
    assert(this->is_int());
    return this->i32 >> 1; 
  }

  template<typename T>
  auto Val::unwrap_obj() const -> T* {
    assert(this->is_obj());
    return reinterpret_cast<T*>(this->u32 & (~0b11));
  }

  auto Val::wrap_int(int32_t num) -> Val {
    assert(((num << 1) >> 1) == num);
    return Val(num << 1); 
  }

  auto Val::wrap_bool(bool b) -> Val {
    return b ? true_val : false_val; 
  }

  auto Val::wrap_fun(void* ptr) -> Val {
    assert(reinterpret_cast<uint32_t>(ptr) % 4 == 0);
    return Val(reinterpret_cast<uint32_t>(ptr) + 0b01);
  }

  template<typename T>
  auto Val::wrap_data_obj(T* ptr) -> Val {
    assert(ptr != 0);
    assert(reinterpret_cast<uint32_t>(ptr) % 4 == 0);
    return Val(reinterpret_cast<uint32_t>(ptr) + 0b11);
  }

  auto Val::get_otable() const -> const ObjTable* {
    assert(this->is_obj());
    auto ptr = this->unwrap_obj<uint32_t>();
    auto otable_ptr = reinterpret_cast<const ObjTable*>(ptr[0]);
    return otable_ptr;
  }
}
#endif
