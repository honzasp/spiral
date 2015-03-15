#ifndef HAVE_spiral_main_hpp
#define HAVE_spiral_main_hpp
#include "spiral/bg.hpp"
#include "spiral/val.hpp"

namespace spiral {
  extern "C" {
    extern auto spiral_start(Bg* bg) -> uint32_t;
    int main(int argc, char** argv);
  }
}

#endif
