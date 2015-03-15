#include <cassert>
#include <cstdlib>
#include <cstdio>
#include "spiral/bg.hpp"

namespace spiral {
  auto bg_heap_alloc(Bg* bg, void*, uint32_t len) -> void* {
    assert(len % 4 == 0);
    if(bg->heap_free_len >= len) {
      auto ptr = static_cast<void*>(bg->heap_ptr);
      bg->heap_ptr += len;
      return ptr;
    } else {
      bg_panic(bg, "out of memory");
    }
  }

  [[noreturn]]
  void bg_panic(Bg*, const char* msg) {
    std::fprintf(stderr, "Panic: %s\n", msg);
    std::abort();
  }
}
