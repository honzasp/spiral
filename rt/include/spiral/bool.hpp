#ifndef HAVE_spiral_bool_hpp
#define HAVE_spiral_bool_hpp
#include "spiral/print.hpp"
#include "spiral/val.hpp"

namespace spiral {
  struct BoolObj {
    const ObjTable* otable;
  };

  void bool_print(Bg* bg, FILE* stream, Val val);

  const ObjTable bool_otable = {
    "bool",
    &bool_print,
  };

  extern "C" {
    extern const BoolObj spiral_true_obj = { &bool_otable };
    extern const BoolObj spiral_false_obj = { &bool_otable };
  }
}
#endif
