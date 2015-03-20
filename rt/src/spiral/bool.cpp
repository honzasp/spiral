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

  void bool_print(Bg*, FILE* stream, Val val) {
    assert(val == Val::true_val || val == Val::false_val);
    if(val == Val::false_val) {
      std::fprintf(stream, "false");
    } else {
      std::fprintf(stream, "true");
    }
  }

  auto bool_length(void* obj_ptr) -> uint32_t {
    assert(obj_ptr == &spiral_true_obj || obj_ptr == &spiral_false_obj);
    return sizeof(BoolObj);
  }

  auto bool_evacuate(GcCtx*, void* obj_ptr) -> Val {
    assert(obj_ptr == &spiral_true_obj || obj_ptr == &spiral_false_obj);
    return Val::wrap_obj(obj_ptr);
  }

  void bool_scavenge(GcCtx* gc_ctx, void*) {
    bg_panic(gc_ctx->bg, "bool scavenged");
  }

  void bool_drop(Bg* bg, void*) {
    bg_panic(bg, "bool dropped");
  }

  const Val Val::true_val = Val(reinterpret_cast<uint32_t>(&spiral_true_obj) + 0b11);
  const Val Val::false_val = Val(reinterpret_cast<uint32_t>(&spiral_false_obj) + 0b11);
}
