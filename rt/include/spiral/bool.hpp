#ifndef HAVE_spiral_bool_hpp
#define HAVE_spiral_bool_hpp
#include "spiral/print.hpp"
#include "spiral/val.hpp"

namespace spiral {
  struct BoolObj {
    const ObjTable* otable;

    static const ObjTable bool_otable;
    static void print(FILE* stream, Val val);
  };

  extern "C" {
    extern const BoolObj spiral_true_obj = { &BoolObj::bool_otable };
    extern const BoolObj spiral_false_obj = { &BoolObj::bool_otable };
  }
}
#endif
