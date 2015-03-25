#include "spiral/fun.hpp"
#include "spiral/gc.hpp"

namespace spiral {
  auto fun_from_val(Bg* bg, Val val) -> FunObj* {
    if(val.is_fun()) {
      return reinterpret_cast<FunObj*>(val.unwrap_obj());
    } else {
      bg_panic(bg, "expected fun");
    }
  }

  auto fun_from_obj_ptr(void* obj_ptr) -> FunObj* {
    assert(reinterpret_cast<uint32_t>(obj_ptr) % 4 == 0);
    auto otable = *reinterpret_cast<const ObjTable**>(obj_ptr);
    assert(otable == &spiral_closure_otable || otable == &spiral_combinator_otable);
    return reinterpret_cast<FunObj*>(obj_ptr);
  }

  auto fun_to_val(FunObj* fun) -> Val {
    return Val::wrap_fun(reinterpret_cast<uint32_t*>(fun));
  }

  auto fun_table_from_addr(void* fun_addr) -> const FunTable* {
    auto table_addr = reinterpret_cast<uint32_t>(fun_addr) - 16;
    assert(table_addr % 16 == 0);
    return reinterpret_cast<const FunTable*>(table_addr);
  }

  void fun_print(Bg*, FILE* stream, Val) {
    std::fprintf(stream, "<fun>");
  }

  auto fun_length(void* obj_ptr) -> uint32_t {
    auto obj = fun_from_obj_ptr(obj_ptr);
    auto ftable = fun_table_from_addr(obj->fun_addr);
    return sizeof(FunObj) + 4 * ftable->capture_count;
  }

  void fun_drop(Bg*, void*) {
  }

  auto closure_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val {
    auto old_fun_obj = fun_from_obj_ptr(obj_ptr);
    auto ftable = fun_table_from_addr(old_fun_obj->fun_addr);
    auto new_fun_obj = static_cast<FunObj*>(gc_get_copy_space(gc_ctx,
          sizeof(FunObj) + 4 * ftable->capture_count));
    new_fun_obj->otable = &spiral_closure_otable;
    new_fun_obj->fun_addr = old_fun_obj->fun_addr;
    for(uint32_t i = 0; i < ftable->capture_count; ++i) {
      new_fun_obj->captures[i] = old_fun_obj->captures[i];
    }

    auto new_val = fun_to_val(new_fun_obj);
    gc_write_fwd_ptr(gc_ctx, obj_ptr, new_val);
    return new_val;
  }

  void closure_scavenge(GcCtx* gc_ctx, void* obj_ptr) {
    auto fun_obj = fun_from_obj_ptr(obj_ptr);
    auto ftable = fun_table_from_addr(fun_obj->fun_addr);
    for(uint32_t i = 0; i < ftable->capture_count; ++i) {
      fun_obj->captures[i] = gc_evacuate(gc_ctx, fun_obj->captures[i]);
    }
  }

  auto combinator_evacuate(GcCtx*, void* obj_ptr) -> Val {
    auto fun_obj = fun_from_obj_ptr(obj_ptr);
    return fun_to_val(fun_obj);
  }

  void combinator_scavenge(GcCtx*, void*) {
  }

  extern "C" {
    const ObjTable spiral_closure_otable = {
      "closure",
      fun_print,
      fun_length,
      closure_evacuate,
      closure_scavenge,
      fun_drop,
    };

    const ObjTable spiral_combinator_otable = {
      "combinator",
      fun_print,
      fun_length,
      combinator_evacuate,
      combinator_scavenge,
      fun_drop,
    };

    auto spiral_alloc_closure(Bg* bg, void* sp, void* fun_addr,
        uint32_t capture_count) -> uint32_t 
    {
      assert(capture_count != 0);
      auto mem = bg_get_obj_space(bg, sp, sizeof(FunObj) + 4 * capture_count);
      auto fun_obj = static_cast<FunObj*>(mem);
      fun_obj->otable = &spiral_closure_otable;
      fun_obj->fun_addr = fun_addr;
      for(uint32_t i = 0; i < capture_count; ++i) {
        fun_obj->captures[i] = Val(0);
      }
      return Val::wrap_fun(fun_obj).u32;
    }
  }
}
