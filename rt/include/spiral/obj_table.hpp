#ifndef HAVE_spiral_obj_table_hpp
#define HAVE_spiral_obj_table_hpp
#include <cstdio>
#include "spiral/bg.hpp"
#include "spiral/val.hpp"

namespace spiral {
  struct ObjTable {
    const char* type_name;
    void (*print_fun)(Bg* bg, FILE* stream, Val val);
  };
}

#endif
