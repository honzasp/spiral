#ifndef HAVE_spiral_buffer_hpp
#define HAVE_spiral_buffer_hpp
#include "spiral/core.hpp"

namespace spiral {
  struct Buffer {
    uint32_t length;
    uint32_t capacity;
    uint8_t* data;
  };

  auto buffer_new(Bg* bg) -> Buffer;
  void buffer_drop(Bg* bg, Buffer buf);
  void buffer_push_byte(Bg* bg, Buffer* buf, uint8_t byte);
  void buffer_push_bytes(Bg* bg, Buffer* buf, const uint8_t* bytes, uint32_t len);
  void buffer_push_cstr(Bg* bg, Buffer* buf, const char* str);
  void buffer_reserve(Bg* bg, Buffer* buf, uint32_t additional);
  void buffer_printf(Bg* bg, Buffer* buf, const char* format, ...);
}
#endif
