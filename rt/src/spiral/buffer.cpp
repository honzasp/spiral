#include <cstdarg>
#include "spiral/buffer.hpp"

namespace spiral {
  auto buffer_new(Bg*) -> Buffer {
    return { 0, 0, 0 };
  }

  void buffer_drop(Bg* bg, Buffer buf) {
    if(buf.data != 0) {
      bg_free_mem(bg, buf.data);
    }
  }

  void buffer_push_byte(Bg* bg, Buffer* buf, uint8_t byte) {
    buffer_reserve(bg, buf, 1);
    buf->data[buf->length++] = byte;
  }

  void buffer_push_bytes(Bg* bg, Buffer* buf, const uint8_t* bytes, uint32_t len) {
    buffer_reserve(bg, buf, len);
    for(uint32_t i = 0; i < len; ++i) {
      buf->data[buf->length++] = bytes[i];
    }
  }

  void buffer_push_cstr(Bg* bg, Buffer* buf, const char* str) {
    uint32_t len = 0;
    while(str[len] != '\0') {
      ++len;
    }
    buffer_reserve(bg, buf, len);
    for(uint32_t i = 0; i < len; ++i) {
      buf->data[buf->length++] = str[i];
    }
  }

  void buffer_reserve(Bg* bg, Buffer* buf, uint32_t additional) {
    if(buf->length + additional > buf->capacity) {
      uint32_t new_capacity = 2 * buf->capacity;
      if(new_capacity < 8) {
        new_capacity = 8;
      }
      while(new_capacity < buf->length + additional) {
        new_capacity *= 2;
      }

      auto new_data = static_cast<uint8_t*>(bg_alloc_mem(bg, new_capacity));
      for(uint32_t i = 0; i < buf->length; ++i) {
        new_data[i] = buf->data[i];
      }
      bg_free_mem(bg, buf->data);
      buf->data = new_data;
    }
  }

  void buffer_printf(Bg* bg, Buffer* buf, const char* format, ...) {
    std::va_list args;
    va_start(args, format);
    int32_t length = std::vsnprintf(0, 0, format, args);
    if(length < 0) {
      bg_panic(bg, "vsnprintf failed");
    }

    buffer_reserve(bg, buf, static_cast<uint32_t>(length) + 1);
    va_start(args, format);
    std::vsnprintf(reinterpret_cast<char*>(&buf->data[buf->length]), length + 1, format, args);
    va_end(args);
    buf->length += length;
  }
}
