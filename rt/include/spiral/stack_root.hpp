#ifndef HAVE_spiral_roots_hpp
#define HAVE_spiral_roots_hpp
#include "spiral/core.hpp"

namespace spiral {
  struct StackRoot {
    StackRoot* next;
    Val value;

    inline StackRoot(Bg* bg, Val value);
    inline ~StackRoot();
    StackRoot() = delete;
    StackRoot(const StackRoot&) = delete;
    StackRoot(StackRoot&&) = delete;

    auto operator=(const StackRoot&) -> StackRoot& = delete;
    auto operator=(StackRoot&&) -> StackRoot& = delete;

    inline auto get() const -> Val;
    inline void set(Val val);
    inline auto unroot(Bg* bg) -> Val;
  };

  StackRoot::StackRoot(Bg* bg, Val value):
    next(bg->top_stack_root), value(value)
  {
    bg->top_stack_root = this;
  }

  StackRoot::~StackRoot() {
    assert(this->next == 0);
  }

  inline auto StackRoot::get() const -> Val {
    return this->value;
  }

  inline void StackRoot::set(Val val) {
    this->value = val;
  }

  auto StackRoot::unroot(Bg* bg) -> Val {
    auto orig = this->value;
    assert(bg->top_stack_root == this);
    bg->top_stack_root = this->next;
    this->next = 0;
    this->value = Val(666);
    return orig;
  }
}
#endif
