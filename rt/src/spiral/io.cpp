#include <cctype>
#include "spiral/buffer.hpp"
#include "spiral/gc.hpp"
#include "spiral/io.hpp"
#include "spiral/number.hpp"
#include "spiral/number_parsers.hpp"
#include "spiral/string.hpp"

namespace spiral {
  const ObjTable io_otable = {
    "io",
    &io_stringify,
    &io_length,
    &io_evacuate,
    &io_scavenge,
    &io_drop,
    &io_eqv,
    &io_eqv,
  };

  auto io_from_val(Bg* bg, Val val) -> IoObj* {
    if(val.is_obj() && val.get_otable() == &io_otable) {
      return val.unwrap_obj<IoObj>();
    } else {
      bg_panic(bg, "expected io");
    }
  }

  auto io_from_obj_ptr(void* obj_ptr) -> IoObj* {
    assert(reinterpret_cast<uint32_t>(obj_ptr) % 4 == 0);
    assert(*reinterpret_cast<const ObjTable**>(obj_ptr) == &io_otable);
    return reinterpret_cast<IoObj*>(obj_ptr);
  }

  auto io_new_from_stdio(Bg* bg, void* sp, std::FILE* stdio, bool close_on_drop) -> IoObj* {
    auto io_obj = static_cast<IoObj*>(bg_get_obj_space(bg, sp, sizeof(IoObj)));
    io_obj->otable = &io_otable;
    io_obj->stdio = stdio;
    io_obj->close_on_drop = close_on_drop;
    return io_obj;
  }

  void io_stringify(Bg* bg, Buffer* buf, void*) {
    buffer_push_cstr(bg, buf, "<io>");
  }

  auto io_length(void*) -> uint32_t {
    return sizeof(IoObj);
  }

  auto io_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val {
    auto old_obj = io_from_obj_ptr(obj_ptr);
    auto new_obj = static_cast<IoObj*>(gc_get_copy_space(gc_ctx, sizeof(IoObj)));
    new_obj->otable = &io_otable;
    new_obj->stdio = old_obj->stdio;
    new_obj->close_on_drop = old_obj->close_on_drop;
    auto new_val = Val::wrap_data_obj(new_obj);
    gc_write_fwd_ptr(gc_ctx, obj_ptr, new_val);
    return new_val;
  }

  void io_scavenge(GcCtx*, void*) {
  }

  void io_drop(Bg*, void* obj_ptr) {
    auto io_obj = io_from_obj_ptr(obj_ptr);
    if(io_obj->close_on_drop) {
      std::fclose(io_obj->stdio);
    } 
  }

  auto io_eqv(Bg*, void* l_ptr, void* r_ptr) -> bool {
    auto l_obj = io_from_obj_ptr(l_ptr);
    auto r_obj = io_from_obj_ptr(r_ptr);
    return l_obj->stdio == r_obj->stdio;
  }

  auto io_open_file(Bg* bg, void* sp, StrObj* path, const char* mode) -> Val {
    auto path_buf = buffer_new(bg);
    buffer_push_bytes(bg, &path_buf, path->data, path->length);
    buffer_push_byte(bg, &path_buf, '\0');

    auto stdio = std::fopen(reinterpret_cast<const char*>(path_buf.data), mode);
    buffer_drop(bg, path_buf);

    if(stdio != 0) {
      auto io_obj = static_cast<IoObj*>(bg_get_obj_space(bg, sp, sizeof(IoObj)));
      io_obj->otable = &io_otable;
      io_obj->stdio = stdio;
      io_obj->close_on_drop = true;
      return Val::wrap_data_obj(io_obj);
    } else {
      return false_val;
    }
  }

  extern "C" {
    auto spiral_std_io_file_open(Bg* bg, void* sp, uint32_t path) -> uint32_t {
      auto path_str = str_from_val(bg, Val(path));
      return io_open_file(bg, sp, path_str, "rb").u32;
    }

    auto spiral_std_io_file_create(Bg* bg, void* sp, uint32_t path) -> uint32_t {
      auto path_str = str_from_val(bg, Val(path));
      return io_open_file(bg, sp, path_str, "wb").u32;
    }

    auto spiral_std_io_file_append(Bg* bg, void* sp, uint32_t path) -> uint32_t {
      auto path_str = str_from_val(bg, Val(path));
      return io_open_file(bg, sp, path_str, "ab").u32;
    }
      
    auto spiral_std_io_close(Bg* bg, void*, uint32_t io) -> uint32_t {
      auto io_obj = io_from_val(bg, Val(io));
      auto fclose_ok = std::fclose(io_obj->stdio) == 0;
      io_obj->close_on_drop = false;
      return Val::wrap_bool(fclose_ok).u32;
    }

    auto spiral_std_io_flush(Bg* bg, void*, uint32_t io) -> uint32_t {
      auto io_obj = io_from_val(bg, Val(io));
      return Val::wrap_bool(std::fflush(io_obj->stdio) == 0).u32;
    }


    auto spiral_std_is_io(Bg*, void*, uint32_t io) -> uint32_t {
      auto io_val = Val(io);
      return Val::wrap_bool(io_val.is_obj() && io_val.get_otable() == &io_otable).u32;
    }

    auto spiral_std_io_is_eof(Bg* bg, void*, uint32_t io) -> uint32_t {
      auto io_obj = io_from_val(bg, Val(io));
      return Val::wrap_bool(std::feof(io_obj->stdio) != 0).u32;
    }

    auto spiral_std_io_is_error(Bg* bg, void*, uint32_t io) -> uint32_t {
      auto io_obj = io_from_val(bg, Val(io));
      return Val::wrap_bool(std::ferror(io_obj->stdio) != 0).u32;
    }

    auto spiral_std_io_stdin(Bg* bg, void* sp) -> uint32_t {
      return Val::wrap_data_obj(io_new_from_stdio(bg, sp, stdin, false)).u32;
    }

    auto spiral_std_io_stdout(Bg* bg, void* sp) -> uint32_t {
      return Val::wrap_data_obj(io_new_from_stdio(bg, sp, stdout, false)).u32;
    }

    auto spiral_std_io_stderr(Bg* bg, void* sp) -> uint32_t {
      return Val::wrap_data_obj(io_new_from_stdio(bg, sp, stderr, false)).u32;
    }

    auto spiral_std_io_write_byte(Bg* bg, void*, uint32_t io, uint32_t byte) -> uint32_t {
      auto io_obj = io_from_val(bg, Val(io));
      auto byte_int = int_from_val(bg, Val(byte));
      return Val::wrap_bool(std::fputc(byte_int, io_obj->stdio) == byte_int).u32;
    }

    auto spiral_std_io_write(Bg* bg, void*, uint32_t io, uint32_t str) -> uint32_t {
      auto io_obj = io_from_val(bg, Val(io));
      auto str_obj = str_from_val(bg, Val(str));
      return Val::wrap_bool(std::fwrite(str_obj->data, str_obj->length,
            1, io_obj->stdio) == str_obj->length).u32;
    }

    auto spiral_std_io_write_line(Bg* bg, void*, uint32_t io, uint32_t str) -> uint32_t {
      auto io_obj = io_from_val(bg, Val(io));
      auto str_obj = str_from_val(bg, Val(str));
      auto fwrite_ok = std::fwrite(str_obj->data, str_obj->length, 1,
          io_obj->stdio) == str_obj->length;
      auto newline_ok = std::fputc('\n', io_obj->stdio) == '\n';
      return Val::wrap_bool(fwrite_ok && newline_ok).u32;
    }

    auto spiral_std_io_read_byte(Bg* bg, void*, uint32_t io) -> uint32_t {
      auto io_obj = io_from_val(bg, Val(io));
      auto byte = std::fgetc(io_obj->stdio);
      if(byte != EOF) {
        return Val::wrap_int(byte).u32;
      } else {
        return false_val.u32;
      }
    }

    auto spiral_std_io_read_str(Bg* bg, void* sp, uint32_t io, uint32_t len) -> uint32_t {
      auto io_obj = io_from_val(bg, Val(io));
      auto len_int = int_from_val(bg, Val(len));
      if(len_int < 0) {
        bg_panic(bg, "cannot read string of negative length");
      }
      auto len_uint = static_cast<uint32_t>(len_int);

      auto str_data = static_cast<uint8_t*>(bg_alloc_mem(bg, len_uint));
      uint32_t str_len = 0;
      while(str_len < len_uint) {
        auto ch = std::fgetc(io_obj->stdio);
        if(ch == EOF) {
          break;
        } else {
          str_data[str_len++] = static_cast<uint8_t>(ch);
        }
      }

      auto str_obj = static_cast<StrObj*>(bg_get_obj_space(bg, sp, sizeof(StrObj)));
      str_obj->otable = &str_otable;
      str_obj->length = str_len;
      str_obj->data = str_data;
      return Val::wrap_data_obj(str_obj).u32;
    }

    auto spiral_std_io_read_all_str(Bg* bg, void* sp, uint32_t io) -> uint32_t {
      auto io_obj = io_from_val(bg, Val(io));
      auto buf = buffer_new(bg);

      for(;;) {
        auto ch = std::fgetc(io_obj->stdio);
        if(ch == EOF) {
          break;
        }
        buffer_push_byte(bg, &buf, static_cast<uint8_t>(ch));
      }

      return Val::wrap_data_obj(str_from_buffer(bg, sp, buf)).u32;
    }

    auto spiral_std_io_read_line(Bg* bg, void* sp, uint32_t io) -> uint32_t {
      auto io_obj = io_from_val(bg, Val(io));
      auto buf = buffer_new(bg);
      for(;;) {
        auto ch = std::fgetc(io_obj->stdio);
        if(ch == EOF) {
          break;
        }
        buffer_push_byte(bg, &buf, static_cast<uint8_t>(ch));
        if(ch == '\n') {
          break;
        }
      }

      return Val::wrap_data_obj(str_from_buffer(bg, sp, buf)).u32;
    }

    auto spiral_std_io_read_word(Bg* bg, void* sp, uint32_t io) -> uint32_t {
      auto io_obj = io_from_val(bg, Val(io));
      auto buf = buffer_new(bg);

      for(;;) {
        auto ch = std::fgetc(io_obj->stdio);
        if(ch == EOF) {
          break;
        }
        if(!std::isspace(ch)) {
          std::ungetc(ch, io_obj->stdio);
          break;
        }
      }

      for(;;) {
        auto ch = std::fgetc(io_obj->stdio);
        if(ch == EOF) {
          break;
        } else if(!std::isspace(ch)) {
          buffer_push_byte(bg, &buf, static_cast<uint8_t>(ch));
        } else {
          std::ungetc(ch, io_obj->stdio);
          break;
        }
      }

      return Val::wrap_data_obj(str_from_buffer(bg, sp, buf)).u32;
    }

    auto spiral_std_io_read_int(Bg* bg, void*, uint32_t io) -> uint32_t {
      auto io_obj = io_from_val(bg, Val(io));

      auto parser = IntParser();
      for(;;) {
        auto ch = std::fgetc(io_obj->stdio);
        if(ch == EOF) {
          break;
        }
        if(!parser.push(static_cast<uint8_t>(ch))) {
          std::ungetc(ch, io_obj->stdio);
          break;
        }
      }

      int32_t number;
      if(parser.get(&number)) {
        return Val::wrap_int(number).u32;
      } else {
        return false_val.u32;
      }
    }

    auto spiral_std_io_read_number(Bg* bg, void* sp, uint32_t io) -> uint32_t {
      auto io_obj = io_from_val(bg, Val(io));

      auto parser = FloatParser();
      for(;;) {
        auto ch = std::fgetc(io_obj->stdio);
        if(ch == EOF) {
          break;
        }
        if(!parser.push(static_cast<uint8_t>(ch))) {
          std::ungetc(ch, io_obj->stdio);
          break;
        }
      }

      double number;
      if(parser.get(&number)) {
        return double_new(bg, sp, number).u32;
      } else {
        return false_val.u32;
      }
    }
  }
}
