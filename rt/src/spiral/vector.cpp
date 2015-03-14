#include <cstdlib>
#include "spiral/main.hpp"
#include "spiral/print.hpp"
#include "spiral/vector.hpp"

namespace spiral {
  auto vector_from_val(Val val) -> VectorObj* {
    if(val.is_obj() && val.get_otable() == &vector_otable) {
      return reinterpret_cast<VectorObj*>(val.unwrap_obj());
    } else {
      panic("expected vector");
    }
  }

  auto vector_to_val(VectorObj* obj) -> Val {
    return Val::wrap_obj(reinterpret_cast<uint32_t*>(obj));
  }

  void vector_print(FILE* stream, Val val) {
    auto vec_obj = vector_from_val(val);

    std::fprintf(stream, "[");
    for(auto i = 0; i < vec_obj->length; ++i) {
      if(i != 0) {
        std::fprintf(stream, " ");
      }
      print(stream, vec_obj->data[i]);
    }
    std::fprintf(stream, "]");
  }

  extern "C" {
    auto spiral_ext_vec_make(uint32_t len_) -> uint32_t {
      auto len_val = Val(len_);
      if(!len_val.is_int()) {
        panic("vector length must be an int");
      }
      if(len_val.unwrap_int() < 0) {
        panic("vector length must be non-negative");
      }

      auto len = len_val.unwrap_int();
      auto memory = std::malloc(sizeof(VectorObj) + sizeof(uint32_t) * len);
      assert(memory != 0);

      auto vec_obj = static_cast<VectorObj*>(memory);
      vec_obj->otable = &vector_otable;
      vec_obj->length = len;
      for(auto i = 0; i < len; ++i) {
        vec_obj->data[i] = Val::false_val;
      }

      return vector_to_val(vec_obj).u32;
    }

    auto spiral_ext_vec_length(uint32_t vec_) -> uint32_t {
      auto vec_obj = vector_from_val(Val(vec_));
      return Val::wrap_int(vec_obj->length).u32;
    }

    auto spiral_ext_vec_get(uint32_t vec_, uint32_t idx_) -> uint32_t {
      auto vec_obj = vector_from_val(Val(vec_));
      auto idx_val = Val(idx_);
      if(!idx_val.is_int()) {
        panic("only ints can index vector");
      }

      auto idx = idx_val.unwrap_int();
      if(idx >= 0 && idx < vec_obj->length) {
        return vec_obj->data[idx].u32;
      } else {
        panic("get index out of bounds");
      }
    }

    auto spiral_ext_vec_set(uint32_t vec_, uint32_t idx_, uint32_t val_) -> uint32_t {
      auto vec_obj = vector_from_val(Val(vec_));
      auto idx_val = Val(idx_);
      if(!idx_val.is_int()) {
        panic("only ints can index vector");
      }

      auto idx = idx_val.unwrap_int();
      if(idx >= 0 && idx < vec_obj->length) {
        return vec_obj->data[idx].u32 = val_;
      } else {
        panic("set index out of bounds");
      }
    }
  }
}
