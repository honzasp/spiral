#include "spiral/bool.hpp"
#include "spiral/gc.hpp"

namespace spiral {
  const ObjTable bool_otable = {
    "bool",
    &bool_stringify,
    &bool_length,
    &bool_evacuate,
    &bool_scavenge,
    &bool_drop,
    &bool_eqv,
    &bool_eqv,
  };

  const BoolObj true_obj = { &bool_otable };
  const BoolObj false_obj = { &bool_otable };
  const Val true_val = Val(reinterpret_cast<uint32_t>(&true_obj) + 0b11);
  const Val false_val = Val(reinterpret_cast<uint32_t>(&false_obj) + 0b11);

  void bool_stringify(Bg* bg, Buffer* buf, void* obj_ptr) {
    assert(obj_ptr == true_val.unwrap_obj<void>() 
        || obj_ptr == false_val.unwrap_obj<void>());

    if(obj_ptr == false_val.unwrap_obj<void>()) {
      buffer_push_cstr(bg, buf, "false");
    } else {
      buffer_push_cstr(bg, buf, "true");
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

  auto bool_eqv(Bg*, void* l_ptr, void* r_ptr) -> bool {
    return l_ptr == r_ptr;
  }
}
