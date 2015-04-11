#include <cstdio>
#include <cstdlib>
#include "spiral/asm_interface.hpp"
#include "spiral/fun.hpp"
#include "spiral/main.hpp"
#include "spiral/print.hpp"

namespace spiral {
  extern "C" {
    int main(int argc, char** argv) {
      auto bg = bg_init(argc, argv);
      fun_addr_call(&bg, spiral_start_addr, 0);
      bg_deinit(&bg);
      return 0;
    }
  }
}
