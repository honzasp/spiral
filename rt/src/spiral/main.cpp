#include <cstdio>
#include <cstdlib>
#include "spiral/main.hpp"

namespace spiral {
  [[noreturn]]
  void panic(const char* msg)
  {
    std::fprintf(stderr, "Panic: %s\n", msg);
    std::abort();
  }

  extern "C" {
    int main(int, char**)
    {
      spiral_start();
      return 0;
    }
  }
}
