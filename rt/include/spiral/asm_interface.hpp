#ifndef HAVE_spiral_asm_interface_hpp
#define HAVE_spiral_asm_interface_hpp
#include "spiral/core.hpp"

namespace spiral {
  extern "C" {
    extern void* spiral_start_addr;
    extern auto spiral_rt_call_fun(Bg* bg, void* fun_addr, void* last_sp) -> uint32_t;
  }
}

#endif
