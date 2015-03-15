#include <cstdio>
#include "spiral/frame_info.hpp"

namespace spiral {
  void stack_dump(void* sp) {
    auto stack_ptr = reinterpret_cast<uint8_t*>(sp);
    for(;;) {
      std::printf("stack ptr %p\n", stack_ptr);
      auto frame_info_ptr = *reinterpret_cast<FrameInfo**>(stack_ptr);
      if(frame_info_ptr == frame_info_skip) {
        auto next_stack_ptr = *reinterpret_cast<uint8_t**>(stack_ptr + 4);
        if(next_stack_ptr == 0) {
          std::printf("%p: end of stack\n", stack_ptr);
          break;
        } else {
          std::printf("%p: skip to %p\n", stack_ptr, next_stack_ptr);
          stack_ptr = next_stack_ptr;
        }
      } else {
        auto slot_count = frame_info_ptr->slot_count;
        auto fun_name = frame_info_ptr->fun_name;
        std::printf("%p: fun %s, %u slots\n", stack_ptr, fun_name, slot_count);
        stack_ptr = stack_ptr + 8 + 4 * slot_count;
      }
    }
  }

  extern "C" {
    auto spiral_ext_debug_stack_dump(Bg*, void* sp) -> uint32_t {
      stack_dump(sp);
      return Val::true_val.u32;
    }
  }
}
