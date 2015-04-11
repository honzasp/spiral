#include <cctype>
#include <cmath>
#include "spiral/number_parsers.hpp"

namespace spiral {
  IntParser::IntParser():
    state(BEFORE_SIGN),
    number(0),
    negative(false)
  { }

  auto IntParser::push(uint8_t ch) -> bool {
    switch(this->state) {
      case BEFORE_SIGN:
        if(ch == '+') { this->state = BEFORE_DIGITS; return true; }
        if(ch == '-') { this->negative = true; this->state = BEFORE_DIGITS; return true; }
      case BEFORE_DIGITS:
        if(std::isspace(ch)) { return true; }
      case DIGITS:
        if(ch >= '0' && ch <= '9') {
          this->state = DIGITS;
          this->number = 10 * this->number + static_cast<int32_t>(ch - '0');
          return true;
        }
        if(std::isspace(ch)) { this->state = AFTER_DIGITS; return true; }
        return false;

      case AFTER_DIGITS:
        if(std::isspace(ch)) { return true; }
        return false;
    }
  }

  auto IntParser::get(int32_t* dest) const -> bool {
    if(this->state == DIGITS || this->state == AFTER_DIGITS) {
      *dest = this->negative ? -this->number : this->number;
      return true;
    } else {
      return false;
    }
  }

  FloatParser::FloatParser():
    state(BEFORE_SIGN),
    whole_part(0.0),
    frac_part(0.0),
    fraction(1.0),
    negative(false),
    exponent(0),
    exponent_negative(false)
  { }

  auto FloatParser::push(uint8_t ch) -> bool {
    switch(this->state) {
      case BEFORE_SIGN:
        if(ch == '+') { this->state = BEFORE_WHOLE; return true; }
        if(ch == '-') {
          this->negative = true;
          this->state = BEFORE_WHOLE;
          return true; 
        }
      case BEFORE_WHOLE:
        if(ch == '.') { this->state = FRAC_DIGITS; return true; }
        if(std::isspace(ch)) { return true; }
      case WHOLE_DIGITS:
        if(ch >= '0' && ch <= '9') { 
          this->state = WHOLE_DIGITS;
          this->whole_part = 10.0 * this->whole_part + static_cast<double>(ch - '0');
          return true;
        } 
        if(ch == '.') { this->state = FRAC_DIGITS; return true; }
        if(ch == 'e' || ch == 'E') { this->state = EXP_SIGN; return true; }
        if(std::isspace(ch)) { this->state = AFTER; return true; }
        return false;

      case FRAC_DIGITS:
        if(ch >= '0' && ch <= '9') {
          this->fraction = this->fraction * 0.1;
          this->frac_part = this->frac_part + this->fraction * static_cast<double>(ch - '0');
          return true;
        }
        if(ch == 'e' || ch == 'E') { this->state = EXP_SIGN; return true; }
        if(std::isspace(ch)) { this->state = AFTER; return true; }
        return false;

      case EXP_SIGN:
        if(ch == '+') { this->state = EXP_DIGITS; return true; }
        if(ch == '-') {
          this->exponent_negative = true;
          this->state = EXP_DIGITS;
          return true; 
        }
      case EXP_DIGITS:
        if(ch >= '0' && ch <= '9') {
          this->state = EXP_DIGITS;
          this->exponent = 10 * this->exponent + static_cast<int32_t>(ch - '0');
          return true;
        }
        if(std::isspace(ch)) { this->state = AFTER; return true; }
        return false;

      case AFTER:
        if(std::isspace(ch)) { return true; }
        return false;
    }
  }

  auto FloatParser::get(double* dest) const -> bool {
    if(this->state == WHOLE_DIGITS || this->state == FRAC_DIGITS) {
      double base = this->whole_part + this->frac_part;
      *dest = this->negative ? -base : base;
      return true;
    } else if(this->state == EXP_DIGITS || this->state == AFTER) {
      double base = this->whole_part + this->frac_part;
      double mult = std::pow(10.0, static_cast<double>(
            this->exponent_negative ? -this->exponent : this->exponent));
      *dest = (this->negative ? -base : base) * mult;
      return true;
    } else {
      return false;
    }
  }
}
