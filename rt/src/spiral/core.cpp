#include <cassert>
#include <cstdlib>
#include <cstdio>
#include "spiral/core.hpp"
#include "spiral/gc.hpp"

namespace spiral {

  auto bg_alloc_mem(Bg* bg, uint32_t len) -> void* {
    if(auto mem = std::malloc(len)) {
      return mem;
    } else {
      bg_panic(bg, "out of memory");
    }
  }

  void bg_free_mem(Bg*, void* mem) {
    std::free(mem);
  }

  auto bg_alloc_chunk(Bg* bg, uint32_t min_len) -> Chunk* {
    uint32_t chunk_total_len = 4 * 1024;
    while(min_len + sizeof(Chunk) > chunk_total_len) {
      chunk_total_len *= 2;
    }

    auto chunk = static_cast<Chunk*>(bg_alloc_mem(bg, chunk_total_len));
    chunk->next_chunk = 0;
    chunk->length = 0;
    chunk->capacity = chunk_total_len - sizeof(Chunk);
    return chunk;
  }

  void bg_free_chunk(Bg* bg, Chunk* chunk) {
    bg_free_mem(bg, chunk);
  }

  auto bg_get_obj_space(Bg* bg, void* sp, uint32_t len) -> void* {
    auto heap_chunk = bg->heap_chunk;
    while(heap_chunk->length + len > heap_chunk->capacity) {
      if(bg->allocated_bytes > 3 * bg->last_alive_bytes) {
        gc_collect(bg, sp);
        heap_chunk = bg->heap_chunk;
      } else {
        auto new_chunk = bg_alloc_chunk(bg, len);
        new_chunk->next_chunk = bg->heap_chunk;
        heap_chunk = bg->heap_chunk = new_chunk;
      }
    }

    auto ptr = heap_chunk->memory + heap_chunk->length;
    heap_chunk->length += len;
    bg->allocated_bytes += len;
    return reinterpret_cast<void*>(ptr);
  }

  [[noreturn]]
  void bg_panic(Bg*, const char* msg) {
    std::fprintf(stderr, "Panic: %s\n", msg);
    std::abort();
  }
}
