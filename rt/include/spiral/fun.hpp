#ifndef HAVE_spiral_fun_hpp
#define HAVE_spiral_fun_hpp
#include "spiral/core.hpp"

namespace spiral {
  struct FunObj {
    const ObjTable* otable;
    void* fun_addr;
    Val captures[0];
  };

  struct FunTable {
    uint32_t slot_count;
    uint32_t arg_count;
    uint32_t capture_count;
    const char* fun_name;
    uint8_t padding_0[16];
  };

  auto fun_from_val(Bg* bg, Val val) -> FunObj*;
  auto fun_from_obj_ptr(void* obj_ptr) -> FunObj*;
  auto fun_to_val(FunObj* fun) -> Val;
  auto fun_table_from_addr(void* fun_addr) -> const FunTable*;

  void fun_print(Bg* bg, FILE* stream, Val val);
  auto fun_length(void* obj_ptr) -> uint32_t;
  auto fun_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val;
  void fun_scavenge(GcCtx* gc_ctx, void* obj_ptr);
  void fun_drop(Bg* bg, void* obj_ptr);
  auto fun_eqv(Bg* bg, void* l_ptr, void* r_ptr) -> bool;
  
  auto fun_addr_call(Bg* bg, const void* fun_addr, void* last_sp) -> Val;
  void panic_invalid_fun(Bg* bg, uint32_t val);
  void panic_argc_mismatch(Bg* bg, void* fun_addr,
      uint32_t expected_argc_, uint32_t received_argc_);

  extern const ObjTable fun_otable;

  extern "C" {
    auto spiral_rt_alloc_closure(Bg* bg, void* sp, void* fun_addr,
        uint32_t capture_count) -> uint32_t;
  }
}
#endif
