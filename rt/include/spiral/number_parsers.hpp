#ifndef HAVE_spiral_parse_numbers_hpp
#define HAVE_spiral_parse_numbers_hpp
#include "spiral/core.hpp"

namespace spiral {
  class IntParser {
    enum {
      BEFORE_SIGN, BEFORE_DIGITS, DIGITS, AFTER_DIGITS, 
    } state;
    int32_t number;
    bool negative;
  public:
    IntParser();
    auto push(uint8_t byte) -> bool;
    auto get(int32_t* dest) const -> bool;
  };

  class FloatParser {
    enum {
      BEFORE_SIGN, BEFORE_WHOLE, WHOLE_DIGITS,
      FRAC_DIGITS, EXP_SIGN, EXP_DIGITS,
      AFTER,
    } state;
    double whole_part;
    double frac_part;
    double fraction;
    bool negative;
    int32_t exponent;
    bool exponent_negative;
  public:
    FloatParser();
    auto push(uint8_t byte) -> bool;
    auto get(double* dest) const -> bool;
  };
}
#endif
