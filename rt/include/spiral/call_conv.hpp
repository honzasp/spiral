#ifndef HAVE_spiral_call_conv_hpp
#define HAVE_spiral_call_conv_hpp
#include "spiral/core.hpp"

namespace spiral {
  extern "C" {
    auto spiral_call_fun(Bg* bg, void* fun_addr, void* last_sp) -> uint32_t;
  }
}
#endif
