#include "spiral/bool.hpp"

namespace spiral {
  void bool_print(Bg*, FILE* stream, Val val)
  {
    assert(val == Val::true_val || val == Val::false_val);
    if(val == Val::false_val) {
      std::fprintf(stream, "false");
    } else {
      std::fprintf(stream, "true");
    }
  }

  const Val Val::true_val = Val(reinterpret_cast<uint32_t>(&spiral_true_obj) + 0b11);
  const Val Val::false_val = Val(reinterpret_cast<uint32_t>(&spiral_false_obj) + 0b11);
}
