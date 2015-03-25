#include <cstdio>
#include <cstdlib>
#include "spiral/call_conv.hpp"
#include "spiral/main.hpp"
#include "spiral/print.hpp"

namespace spiral {
  extern "C" {
    int main(int, char**) {
      Bg bg;
      bg.heap_chunk = 0;
      bg.allocated_bytes = 0;
      bg.last_alive_bytes = 4 * 1024;

      bg.heap_chunk = bg_alloc_chunk(&bg, 0);
      bg.heap_chunk->next_chunk = 0;

      spiral_call_fun(&bg, spiral_start_fun, 0);
      return 0;
    }
  }
}
