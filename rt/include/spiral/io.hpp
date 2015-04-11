#ifndef HAVE_spiral_io_hpp
#define HAVE_spiral_io_hpp
#include <cstdio>
#include "spiral/core.hpp"
#include "spiral/string.hpp"

namespace spiral {
  struct IoObj {
    const ObjTable* otable;
    std::FILE* stdio;
    bool close_on_drop;
  };

  auto io_from_val(Bg* bg, Val val) -> IoObj*;
  auto io_from_obj_ptr(void* obj_ptr) -> IoObj*;
  auto io_new_from_stdio(Bg* bg, void* sp, std::FILE* stdio, bool close_on_drop) -> IoObj*;

  void io_stringify(Bg* bg, Buffer* buf, void* obj_ptr);
  auto io_length(void* obj_ptr) -> uint32_t;
  auto io_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val;
  void io_scavenge(GcCtx* gc_ctx, void* obj_ptr);
  void io_drop(Bg* bg, void* obj_ptr);
  auto io_eqv(Bg* bg, void* l_ptr, void* r_ptr) -> bool;

  extern const ObjTable io_otable;

  auto io_open_file(Bg* bg, void* sp, StrObj* path, const char* mode) -> Val;

  extern "C" {
    auto spiral_std_io_file_open(Bg* bg, void* sp, uint32_t path) -> uint32_t;
    auto spiral_std_io_file_create(Bg* bg, void* sp, uint32_t path) -> uint32_t;
    auto spiral_std_io_file_append(Bg* bg, void* sp, uint32_t path) -> uint32_t;
    auto spiral_std_io_close(Bg* bg, void* sp, uint32_t io) -> uint32_t;
    auto spiral_std_io_flush(Bg* bg, void* sp, uint32_t io) -> uint32_t;

    auto spiral_std_is_io(Bg* bg, void* sp, uint32_t io) -> uint32_t;
    auto spiral_std_io_is_eof(Bg* bg, void* sp, uint32_t io) -> uint32_t;
    auto spiral_std_io_is_error(Bg* bg, void* sp, uint32_t io) -> uint32_t;

    auto spiral_std_io_stdin(Bg* bg, void* sp) -> uint32_t;
    auto spiral_std_io_stdout(Bg* bg, void* sp) -> uint32_t;
    auto spiral_std_io_stderr(Bg* bg, void* sp) -> uint32_t;

    auto spiral_std_io_write_byte(Bg* bg, void* sp, uint32_t io, uint32_t byte) -> uint32_t;
    auto spiral_std_io_write(Bg* bg, void* sp, uint32_t io, uint32_t str) -> uint32_t;
    auto spiral_std_io_write_line(Bg* bg, void* sp, uint32_t io, uint32_t str) -> uint32_t;

    auto spiral_std_io_read_byte(Bg* bg, void* sp, uint32_t io) -> uint32_t;
    auto spiral_std_io_read_str(Bg* bg, void* sp, uint32_t io, uint32_t len) -> uint32_t;
    auto spiral_std_io_read_all_str(Bg* bg, void* sp, uint32_t io) -> uint32_t;
    auto spiral_std_io_read_line(Bg* bg, void* sp, uint32_t io) -> uint32_t;

    auto spiral_std_io_read_word(Bg* bg, void* sp, uint32_t io) -> uint32_t;
    auto spiral_std_io_read_int(Bg* bg, void* sp, uint32_t io) -> uint32_t;
    auto spiral_std_io_read_number(Bg* bg, void* sp, uint32_t io) -> uint32_t;
  }
}
#endif
