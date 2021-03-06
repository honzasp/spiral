#ifndef HAVE_spiral_gc_hpp
#define HAVE_spiral_gc_hpp
#include "spiral/core.hpp"

namespace spiral {
  struct GcCtx {
    Bg* bg;
    Chunk* to_heap_begin;
    Chunk* to_heap_last;
    uint32_t copied_bytes;
    uint32_t non_heap_alive_bytes;
  };

  void gc_collect(Bg* bg, void* sp);
  void gc_evacuate_stack(GcCtx* gc_ctx, void* sp);
  void gc_evacuate_roots(GcCtx* gc_ctx, void* sp);
  auto gc_evacuate(GcCtx* gc_ctx, Val val) -> Val;
  auto gc_get_copy_space(GcCtx* gc_ctx, uint32_t len) -> void*;
  void gc_scavenge_chunk(GcCtx* gc_ctx, Chunk* chunk);
  void gc_drop_chunk(Bg* bg, Chunk* chunk);
  void gc_write_fwd_ptr(GcCtx* gc_ctx, void* obj_ptr, Val new_val);
}
#endif
