#include "spiral/equiv.hpp"
#include "spiral/number.hpp"

namespace spiral {
  template<typename F>
  static auto equivalent(Bg* bg, Val l, Val r, F eq_objs) -> bool;
  static auto eqv_int_obj(Bg* bg, Val i, Val o) -> bool;

  template<typename F>
  static auto equivalent(Bg* bg, Val l, Val r, F eq_objs) -> bool {
    if(l == r) {
      return true;
    } else if(l.is_obj() && r.is_obj()) {
      auto l_otable = l.get_otable();
      auto r_otable = r.get_otable();
      if(l_otable == r_otable) {
        return eq_objs(bg, l_otable, l.unwrap_obj<void>(), r.unwrap_obj<void>());
      } else {
        return false;
      }
    } else if(r.is_int() && l.is_obj()) {
      return eqv_int_obj(bg, r, l);
    } else if(l.is_int() && r.is_obj()) {
      return eqv_int_obj(bg, l, r);
    } else {
      return false;
    }
  }

  auto eqv(Bg* bg, Val l, Val r) -> bool {
    return equivalent(bg, l, r,
        [](Bg* bg, const ObjTable* otable, void* obj_l, void* obj_r) {
          return (*otable->eqv_fun)(bg, obj_l, obj_r);
        });
  }

  auto equal(Bg* bg, Val l, Val r) -> bool {
    return equivalent(bg, l, r,
        [](Bg* bg, const ObjTable* otable, void* obj_l, void* obj_r) {
          return (*otable->equal_fun)(bg, obj_l, obj_r);
        });
  }

  static auto eqv_int_obj(Bg*, Val i, Val o) -> bool {
    assert(i.is_int() && o.is_obj());
    auto otable = o.get_otable();
    if(otable == &double_otable) {
      auto dbl_obj = o.unwrap_obj<DoubleObj>();
      return static_cast<double>(i.unwrap_int()) == dbl_obj->num;
    } else {
      return false;
    }
  }

  extern "C" {
    auto spiral_std_eqv(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return Val::wrap_bool(eqv(bg, Val(a), Val(b))).u32;
    }

    auto spiral_std_equal(Bg* bg, void*, uint32_t a, uint32_t b) -> uint32_t {
      return Val::wrap_bool(equal(bg, Val(a), Val(b))).u32;
    }
  }
}
