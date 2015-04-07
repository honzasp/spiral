#ifndef HAVE_spiral_asm_interface_hpp
#define HAVE_spiral_asm_interface_hpp
#include <cstdint>

namespace spiral {
  struct Bg;
  using std::uint32_t;

  extern "C" {
    extern const void* spiral_start_addr;
    extern const uint32_t spiral_static_begin;
    extern const uint32_t spiral_static_end;
    extern auto spiral_rt_call_fun(Bg* bg, const void* fun_addr,
        void* last_sp) -> uint32_t;
  }
}

#endif
