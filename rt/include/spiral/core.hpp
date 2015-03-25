#ifndef HAVE_spiral_core_hpp
#define HAVE_spiral_core_hpp
#include <cstdio>
#include "spiral/val.hpp"

namespace spiral {
  struct GcCtx;

  struct Chunk {
    Chunk* next_chunk;
    uint32_t length;
    uint32_t capacity;
    uint8_t memory[];
  };

  struct Bg {
    Chunk* heap_chunk;
    uint32_t allocated_bytes;
    uint32_t last_alive_bytes;
  };

  struct ObjTable {
    const char* type_name;
    void (*print_fun)(Bg* bg, FILE* stream, Val val);
    auto (*length_fun)(void* obj_ptr) -> uint32_t;
    auto (*evacuate_fun)(GcCtx* gc_ctx, void* obj_ptr) -> Val;
    void (*scavenge_fun)(GcCtx* gc_ctx, void* obj_ptr);
    void (*drop_fun)(Bg* bg, void* obj_ptr);
  };

  auto bg_alloc_mem(Bg* bg, uint32_t len) -> void*;
  void bg_free_mem(Bg* bg, void* mem);
  auto bg_alloc_chunk(Bg* bg, uint32_t min_len) -> Chunk*;
  void bg_free_chunk(Bg* bg, Chunk* chunk);
  auto bg_get_obj_space(Bg* bg, void* sp, uint32_t len) -> void*;
  
  [[noreturn]] void bg_panic(Bg* bg, const char* msg);
}
#endif
