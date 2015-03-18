#ifndef HAVE_spiral_main_hpp
#define HAVE_spiral_main_hpp
#include "spiral/core.hpp"

namespace spiral {
  extern "C" {
    extern void* spiral_start_fun;
    int main(int argc, char** argv);
  }
}

#endif
