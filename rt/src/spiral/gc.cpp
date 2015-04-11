#include "spiral/fun.hpp"
#include "spiral/fwd_ptr.hpp"
#include "spiral/gc.hpp"
#include "spiral/stack_root.hpp"

namespace spiral {
  void gc_collect(Bg* bg, void* sp) {
    GcCtx gc_ctx;
    gc_ctx.bg = bg;
    gc_ctx.to_heap_chunk = bg_alloc_chunk(bg, 0);
    gc_ctx.to_heap_chunk->next_chunk = 0;
    gc_ctx.copied_bytes = 0;
    gc_ctx.non_heap_alive_bytes = 0;

    gc_evacuate_stack(&gc_ctx, sp);
    gc_evacuate_roots(&gc_ctx, sp);
    for(Chunk* chunk = gc_ctx.to_heap_chunk; chunk != 0; chunk = chunk->next_chunk) {
      gc_scavenge_chunk(&gc_ctx, chunk);
    }

    auto dealloc_chunk = bg->heap_chunk;
    while(dealloc_chunk != 0) {
      auto next_chunk = dealloc_chunk->next_chunk;
      gc_drop_chunk(bg, dealloc_chunk);
      bg_free_chunk(bg, dealloc_chunk);
      dealloc_chunk = next_chunk;
    }

    bg->heap_chunk = gc_ctx.to_heap_chunk;
    bg->fresh_allocated_bytes = 0;
    bg->last_alive_bytes = gc_ctx.copied_bytes + gc_ctx.non_heap_alive_bytes;
  }

  void gc_evacuate_stack(GcCtx* gc_ctx, void* sp) {
    auto stack_ptr = reinterpret_cast<uint32_t>(sp);
    for(;;) {
      assert(stack_ptr % 4 == 0);
      auto fun_ptr = reinterpret_cast<uint32_t*>(stack_ptr + 0);
      if(*fun_ptr == 0xffffffff) {
        auto next_stack_ptr = *reinterpret_cast<uint32_t*>(stack_ptr + 4);
        if(next_stack_ptr == 0) {
          break;
        } else {
          stack_ptr = next_stack_ptr;
        }
      } else {
        *fun_ptr = gc_evacuate(gc_ctx, Val(*fun_ptr)).u32;
        auto fun_obj = fun_from_obj_ptr(reinterpret_cast<FunObj*>(*fun_ptr - 0b01));
        auto ftable = fun_table_from_addr(fun_obj->fun_addr);
        auto slot_count = ftable->slot_count;
        for(uint32_t slot = 0; slot < slot_count; ++slot) {
          auto slot_ptr = reinterpret_cast<uint32_t*>(stack_ptr + 4 + 4 * slot);
          *slot_ptr = gc_evacuate(gc_ctx, Val(*slot_ptr)).u32;
        }
        stack_ptr = stack_ptr + 8 + 4 * slot_count;
      }
    }
  }

  void gc_evacuate_roots(GcCtx* gc_ctx, void*) {
    auto root = gc_ctx->bg->top_stack_root;
    while(root != 0) {
      root->value = gc_evacuate(gc_ctx, root->value);
      root = root->next;
    }
  }

  auto gc_evacuate(GcCtx* gc_ctx, Val val) -> Val {
    if(val.is_int()) {
      return val;
    } else {
      return val.get_otable()->evacuate_fun(gc_ctx, val.unwrap_obj<void>());
    }
  }

  auto gc_get_copy_space(GcCtx* gc_ctx, uint32_t len) -> void* {
    auto target_chunk = gc_ctx->to_heap_chunk;
    if(target_chunk->length + len > target_chunk->capacity) {
      auto new_chunk = bg_alloc_chunk(gc_ctx->bg, len);
      new_chunk->next_chunk = target_chunk;
      target_chunk = gc_ctx->to_heap_chunk = new_chunk;
    }

    auto ptr = target_chunk->memory + target_chunk->length;
    target_chunk->length += len;
    gc_ctx->copied_bytes += len;
    return ptr;
  }

  void gc_scavenge_chunk(GcCtx* gc_ctx, Chunk* chunk) {
    uint32_t index = 0;
    while(index < chunk->length) {
      auto obj_ptr = chunk->memory + index;
      auto obj_table = *reinterpret_cast<const ObjTable**>(obj_ptr);
      obj_table->scavenge_fun(gc_ctx, reinterpret_cast<void*>(obj_ptr));
      index += obj_table->length_fun(reinterpret_cast<void*>(obj_ptr));
    }
  }

  void gc_drop_chunk(Bg* bg, Chunk* chunk) {
    uint32_t index = 0;
    while(index < chunk->length) {
      auto obj_ptr = chunk->memory + index;
      auto obj_table = *reinterpret_cast<const ObjTable**>(obj_ptr);
      index += obj_table->length_fun(reinterpret_cast<void*>(obj_ptr));
      obj_table->drop_fun(bg, reinterpret_cast<void*>(obj_ptr));
    }
  }

  void gc_write_fwd_ptr(GcCtx*, void* obj_ptr, Val new_val) {
    assert(reinterpret_cast<uint32_t>(obj_ptr) % 4 == 0);
    auto fwd_ptr = static_cast<FwdPtrObj*>(obj_ptr);
    fwd_ptr->otable = &fwd_ptr_otable;
    fwd_ptr->fwd = new_val;
  }
}
