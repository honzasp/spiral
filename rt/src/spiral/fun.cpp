#include "spiral/asm_interface.hpp"
#include "spiral/fun.hpp"
#include "spiral/gc.hpp"
#include "spiral/print.hpp"

namespace spiral {
  const ObjTable fun_otable = {
    "fun",
    &fun_stringify,
    &fun_length,
    &fun_evacuate,
    &fun_scavenge,
    &fun_drop,
    &fun_eqv,
    &fun_eqv,
  };

  auto fun_from_val(Bg* bg, Val val) -> FunObj* {
    if(val.is_fun()) {
      return val.unwrap_obj<FunObj>();
    } else {
      bg_panic(bg, "expected fun");
    }
  }

  auto fun_from_obj_ptr(void* obj_ptr) -> FunObj* {
    assert(reinterpret_cast<uint32_t>(obj_ptr) % 4 == 0);
    auto otable = *reinterpret_cast<const ObjTable**>(obj_ptr);
    assert(otable == &fun_otable);
    return reinterpret_cast<FunObj*>(obj_ptr);
  }

  auto fun_to_val(FunObj* fun) -> Val {
    return Val::wrap_fun(reinterpret_cast<uint32_t*>(fun));
  }

  auto fun_table_from_addr(void* fun_addr) -> const FunTable* {
    auto table_addr = reinterpret_cast<uint32_t>(fun_addr) - 32;
    assert(table_addr % 16 == 0);
    return reinterpret_cast<const FunTable*>(table_addr);
  }

  void fun_stringify(Bg* bg, Buffer* buf, void*) {
    buffer_push_cstr(bg, buf, "<fun>");
  }

  auto fun_length(void* obj_ptr) -> uint32_t {
    auto obj = fun_from_obj_ptr(obj_ptr);
    auto ftable = fun_table_from_addr(obj->fun_addr);
    return sizeof(FunObj) + 4 * ftable->capture_count;
  }

  void fun_drop(Bg*, void* obj_ptr) {
    assert(!ptr_is_static(obj_ptr));
  }

  auto fun_eqv(Bg*, void* l_ptr, void* r_ptr) -> bool {
    return l_ptr == r_ptr;
  }

  auto fun_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val {
    if(!ptr_is_static(obj_ptr)) {
      auto old_fun_obj = fun_from_obj_ptr(obj_ptr);
      auto ftable = fun_table_from_addr(old_fun_obj->fun_addr);
      auto new_fun_obj = static_cast<FunObj*>(gc_get_copy_space(gc_ctx,
            sizeof(FunObj) + 4 * ftable->capture_count));
      new_fun_obj->otable = &fun_otable;
      new_fun_obj->fun_addr = old_fun_obj->fun_addr;
      for(uint32_t i = 0; i < ftable->capture_count; ++i) {
        new_fun_obj->captures[i] = old_fun_obj->captures[i];
      }

      auto new_val = fun_to_val(new_fun_obj);
      gc_write_fwd_ptr(gc_ctx, obj_ptr, new_val);
      return new_val;
    } else {
      return Val::wrap_fun(obj_ptr);
    }
  }

  void fun_scavenge(GcCtx* gc_ctx, void* obj_ptr) {
    if(!ptr_is_static(obj_ptr)) {
      auto fun_obj = fun_from_obj_ptr(obj_ptr);
      auto ftable = fun_table_from_addr(fun_obj->fun_addr);
      for(uint32_t i = 0; i < ftable->capture_count; ++i) {
        fun_obj->captures[i] = gc_evacuate(gc_ctx, fun_obj->captures[i]);
      }
    }
  }

  auto fun_addr_call(Bg* bg, const void* fun_addr, void* last_sp) -> Val {
    return Val(spiral_rt_call_fun(bg, fun_addr, last_sp));
  }

  void panic_invalid_fun(Bg* bg, uint32_t val) {
    Buffer buf = buffer_new(bg);
    buffer_push_cstr(bg, &buf, "Expected fun, got ");
    stringify_short(bg, &buf, Val(val));
    buffer_push_byte(bg, &buf, '\0');
    bg_panic(bg, reinterpret_cast<const char*>(buf.data));
  }

  void panic_argc_mismatch(Bg* bg, void* fun_addr,
      uint32_t expected_argc_, uint32_t received_argc_) {
    auto expected_argc = Val(expected_argc_);
    auto received_argc = Val(received_argc_);
    assert(expected_argc.is_int());
    assert(received_argc.is_int());
    std::fprintf(stderr, "Fun %p expected %u args, got %u args\n", fun_addr, 
        expected_argc.unwrap_int(), received_argc.unwrap_int());
    bg_panic(bg, "argc mismatch");
  }

  extern "C" {
    auto spiral_rt_alloc_closure(Bg* bg, void* sp, void* fun_addr,
        uint32_t capture_count) -> uint32_t 
    {
      auto capture_count_val = Val(capture_count);
      assert(capture_count_val.is_int());
      assert(capture_count_val.unwrap_int() > 0);
      assert(fun_table_from_addr(fun_addr)->capture_count ==
          static_cast<uint32_t>(capture_count_val.unwrap_int()));

      auto mem = bg_get_obj_space(bg, sp, sizeof(FunObj) 
          + 4 * capture_count_val.unwrap_int());
      auto fun_obj = static_cast<FunObj*>(mem);
      fun_obj->otable = &fun_otable;
      fun_obj->fun_addr = fun_addr;
      for(uint32_t i = 0; i < capture_count; ++i) {
        fun_obj->captures[i] = Val(0);
      }
      return Val::wrap_fun(fun_obj).u32;
    }
  }
}
