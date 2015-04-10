#include <cstdio>
#include "spiral/bool.hpp"
#include "spiral/cons.hpp"
#include "spiral/equiv.hpp"
#include "spiral/gc.hpp"
#include "spiral/print.hpp"
#include "spiral/stack_root.hpp"

namespace spiral {
  const ObjTable cons_otable = {
    "cons",
    &cons_stringify,
    &cons_length,
    &cons_evacuate,
    &cons_scavenge,
    &cons_drop,
    &cons_eqv,
    &cons_equal,
  };

  auto cons_from_val(Bg* bg, Val val) -> ConsObj* {
    if(val.is_obj() && val.get_otable() == &cons_otable) {
      return val.unwrap_obj<ConsObj>();
    } else {
      bg_panic(bg, "expected cons");
    }
  }

  auto cons_to_val(ConsObj* obj) -> Val {
    return Val::wrap_data_obj(obj);
  }

  auto cons_from_obj_ptr(void* obj_ptr) -> ConsObj* {
    assert(reinterpret_cast<uint32_t>(obj_ptr) % 4 == 0);
    assert(*reinterpret_cast<const ObjTable**>(obj_ptr) == &cons_otable);
    return reinterpret_cast<ConsObj*>(obj_ptr);
  }

  auto cons_new(Bg* bg, void* sp, Val car, Val cdr) -> ConsObj* {
    StackRoot car_root(bg, car);
    StackRoot cdr_root(bg, cdr);
    auto cons_obj = static_cast<ConsObj*>(bg_get_obj_space(bg, sp, sizeof(ConsObj)));
    cons_obj->otable = &cons_otable;
    cons_obj->cdr = cdr_root.unroot(bg);
    cons_obj->car = car_root.unroot(bg);;
    return cons_obj;
  }

  void cons_stringify(Bg* bg, Buffer* buf, void* obj_ptr) {
    buffer_push_byte(bg, buf, '(');

    auto head_val = Val::wrap_data_obj(obj_ptr);
    auto first = true;
    while(head_val.is_obj() && head_val.get_otable() == &cons_otable) {
      auto cons_obj = head_val.unwrap_obj<ConsObj>();
      if(!first) {
        buffer_push_byte(bg, buf, ' ');
      }
      stringify(bg, buf, cons_obj->car);
      head_val = cons_obj->cdr;
      first = false;
    }

    if(head_val != false_val) {
      buffer_push_cstr(bg, buf, " . ");
      stringify(bg, buf, head_val);
    }
    buffer_push_byte(bg, buf, ')');
  }

  auto cons_length(void*) -> uint32_t {
    return sizeof(ConsObj);
  }

  auto cons_evacuate(GcCtx* gc_ctx, void* obj_ptr) -> Val {
    auto old_obj = cons_from_obj_ptr(obj_ptr);
    auto new_obj = static_cast<ConsObj*>(gc_get_copy_space(gc_ctx, sizeof(ConsObj)));
    new_obj->otable = &cons_otable;
    new_obj->car = old_obj->car;
    new_obj->cdr = old_obj->cdr;
    auto new_val = cons_to_val(new_obj);
    gc_write_fwd_ptr(gc_ctx, obj_ptr, new_val);
    return new_val;
  }

  void cons_scavenge(GcCtx* gc_ctx, void* obj_ptr) {
    auto cons_obj = cons_from_obj_ptr(obj_ptr);
    cons_obj->car = gc_evacuate(gc_ctx, cons_obj->car);
    cons_obj->cdr = gc_evacuate(gc_ctx, cons_obj->cdr);
  }

  void cons_drop(Bg*, void*) {
  }

  auto cons_eqv(Bg*, void* l_ptr, void* r_ptr) -> bool {
    return l_ptr == r_ptr;
  }

  auto cons_equal(Bg* bg, void* l_ptr, void* r_ptr) -> bool {
    auto l_cons = static_cast<ConsObj*>(l_ptr);
    auto r_cons = static_cast<ConsObj*>(r_ptr);
    return equal(bg, l_cons->car, r_cons->car) && equal(bg, l_cons->cdr, r_cons->cdr);
  }

  extern "C" {
    auto spiral_std_cons_new(Bg* bg, void* sp,
        uint32_t car, uint32_t cdr) -> uint32_t 
    {
      return cons_to_val(cons_new(bg, sp, Val(car), Val(cdr))).u32;
    }

    auto spiral_std_is_cons(Bg*, void*, uint32_t val_) -> uint32_t {
      auto val = Val(val_);
      return Val::wrap_bool(val.is_obj() && val.get_otable() == &cons_otable).u32;
    }
      
    auto spiral_std_car(Bg* bg, void*, uint32_t cons) -> uint32_t {
      auto cons_obj = cons_from_val(bg, Val(cons));
      return cons_obj->car.u32;
    }

    auto spiral_std_cdr(Bg* bg, void*, uint32_t cons) -> uint32_t {
      auto cons_obj = cons_from_val(bg, Val(cons));
      return cons_obj->cdr.u32;
    }
  }
}
