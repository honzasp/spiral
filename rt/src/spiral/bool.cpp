#include "spiral/bool.hpp"
#include "spiral/gc.hpp"

namespace spiral {
  const ObjTable bool_otable = {
    "bool",
    &bool_print,
    &bool_length,
    &bool_evacuate,
    &bool_scavenge,
    &bool_drop,
  };

  const BoolObj true_obj = { &bool_otable };
  const BoolObj false_obj = { &bool_otable };
  const Val true_val = Val(reinterpret_cast<uint32_t>(&true_obj) + 0b11);
  const Val false_val = Val(reinterpret_cast<uint32_t>(&false_obj) + 0b11);

  void bool_print(Bg*, FILE* stream, Val val) {
    assert(val == true_val || val == false_val);
    if(val == false_val) {
      std::fprintf(stream, "false");
    } else {
      std::fprintf(stream, "true");
    }
  }

  auto bool_length(void* obj_ptr) -> uint32_t {
    assert(obj_ptr == &true_obj || obj_ptr == &false_obj);
    return sizeof(BoolObj);
  }

  auto bool_evacuate(GcCtx*, void* obj_ptr) -> Val {
    assert(obj_ptr == &true_obj || obj_ptr == &false_obj);
    return Val::wrap_data_obj(obj_ptr);
  }

  void bool_scavenge(GcCtx*, void*) {
    assert("bool scavenged");
  }

  void bool_drop(Bg*, void*) {
    assert("bool dropped");
  }
}
