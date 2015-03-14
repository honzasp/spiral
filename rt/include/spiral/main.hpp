#ifndef HAVE_spiral_main_hpp
#define HAVE_spiral_main_hpp
#include "spiral/val.hpp"

namespace spiral {
  [[noreturn]]
  void panic(const char* msg);
  extern "C" {
    extern auto spiral_start() -> uint32_t;
    int main(int argc, char** argv);
  }
}

#endif
