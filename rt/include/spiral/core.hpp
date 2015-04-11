#ifndef HAVE_spiral_core_hpp
#define HAVE_spiral_core_hpp
#include <cstdio>
#include "spiral/val.hpp"

namespace spiral {
  struct GcCtx;
  struct Buffer;
  struct StackRoot;

  struct Chunk {
    Chunk* next_chunk;
    uint32_t length;
    uint32_t capacity;
    uint8_t memory[];
  };

  struct Bg {
    Chunk* heap_chunk;
    uint32_t fresh_allocated_bytes;
    uint32_t last_alive_bytes;
    StackRoot* top_stack_root;
    int argc;
    char** argv;
  };

  struct ObjTable {
    const char* type_name;
    void (*stringify_fun)(Bg* bg, Buffer* buf, void* obj_ptr);
    auto (*length_fun)(void* obj_ptr) -> uint32_t;
    auto (*evacuate_fun)(GcCtx* gc_ctx, void* obj_ptr) -> Val;
    void (*scavenge_fun)(GcCtx* gc_ctx, void* obj_ptr);
    void (*drop_fun)(Bg* bg, void* obj_ptr);
    auto (*eqv_fun)(Bg* bg, void* l, void* r) -> bool;
    auto (*equal_fun)(Bg* bg, void* l_ptr, void* r_ptr) -> bool;
  };

  auto bg_init(int argc, char** argv) -> Bg;
  void bg_deinit(Bg* bg);
  auto bg_alloc_mem(Bg* bg, uint32_t len) -> void*;
  void bg_free_mem(Bg* bg, void* mem);
  auto bg_alloc_chunk(Bg* bg, uint32_t min_len) -> Chunk*;
  void bg_free_chunk(Bg* bg, Chunk* chunk);
  auto bg_get_obj_space(Bg* bg, void* sp, uint32_t len) -> void*;
  
  [[noreturn]] void bg_panic(Bg* bg, const char* msg);
}
#endif
