#include <stdio.h>
#include <stdint.h>

extern void spr_start();

float spiral_ext_println(float x)
{
  printf("%f\n", x);
  return 0.0;
}

static uint32_t trans(float x)
{
  union { float flt; uint32_t u32; } helper;
  helper.flt = x;
  return helper.u32;
}

uint32_t spiral_ext_add(float a, float b) { return trans(a + b); }
uint32_t spiral_ext_sub(float a, float b) { return trans(a - b); }
uint32_t spiral_ext_mul(float a, float b) { return trans(a * b); }
uint32_t spiral_ext_div(float a, float b) { return trans(a / b); }
uint32_t spiral_ext_lt(float a, float b) { return trans(a < b ? 1.0 : 0.0); }
uint32_t spiral_ext_le(float a, float b) { return trans(a <= b ? 1.0 : 0.0); }
uint32_t spiral_ext_eq(float a, float b) { return trans(a == b ? 1.0 : 0.0); }
uint32_t spiral_ext_ne(float a, float b) { return trans(a != b ? 1.0 : 0.0); }
uint32_t spiral_ext_gt(float a, float b) { return trans(a > b ? 1.0 : 0.0); }
uint32_t spiral_ext_ge(float a, float b) { return trans(a >= b ? 1.0 : 0.0); }

int main(int argc, char** argv)
{
  spr_start();
  return 0;
}
