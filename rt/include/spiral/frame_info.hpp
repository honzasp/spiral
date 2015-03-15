#ifndef HAVE_spiral_frame_info_hpp
#define HAVE_spiral_frame_info_hpp
#include "spiral/bg.hpp"

namespace spiral {
  struct FrameInfo {
    uint32_t slot_count;
    const char* fun_name;
  };

  const FrameInfo* const frame_info_skip =
    reinterpret_cast<FrameInfo*>(0xffffffff);

  void stack_dump(void* sp);

  extern "C" {
    auto spiral_ext_debug_stack_dump(Bg* bg, void* sp) -> uint32_t;
  }
}
#endif
