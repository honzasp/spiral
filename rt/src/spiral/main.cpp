#include <cstdio>
#include <cstdlib>
#include "spiral/main.hpp"

namespace spiral {
  extern "C" {
    int main(int, char**) {
      Bg bg;
      bg.heap_free_len = 4 * 1024;
      bg.heap_ptr = static_cast<uint8_t*>(std::malloc(bg.heap_free_len));
      assert(bg.heap_ptr != 0);

      spiral_start(&bg);
      return 0;
    }
  }
}
