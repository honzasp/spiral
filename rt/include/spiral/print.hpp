#ifndef HAVE_spiral_print_hpp
#define HAVE_spiral_print_hpp
#include "spiral/obj_table.hpp"
#include "spiral/val.hpp"

namespace spiral {
  void println(FILE* stream, Val val);
  void print(FILE* stream, Val val);

  extern "C" {
    auto spiral_ext_println(uint32_t val_) -> uint32_t;
  }
}

#endif
