#ifndef HAVE_spiral_bg_hpp
#define HAVE_spiral_bg_hpp
#include "spiral/val.hpp"

namespace spiral {
  struct Bg {
    uint8_t* heap_ptr;
    uint32_t heap_free_len;
  };

  auto bg_heap_alloc(Bg* bg, void* sp, uint32_t len) -> void*;
  [[noreturn]]
  void bg_panic(Bg* bg, const char* msg);
}
#endif
