#ifndef HAVE_spiral_env_hpp
#define HAVE_spiral_env_hpp
#include "spiral/core.hpp"

namespace spiral {
  extern "C" {
    auto spiral_std_env_get_argv(Bg* bg, void* sp) -> uint32_t;
    auto spiral_std_env_get_var(Bg* bg, void* sp, uint32_t var) -> uint32_t;
  }
}
#endif
