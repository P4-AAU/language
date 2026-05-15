#include <stdio.h>
#include <stdint.h>
#include <math.h>
#include <assert.h>

int main(void)
{
  struct { int32_t data[5]; int32_t len; int32_t cap; } b = { {10, 20, 30}, 3, 5 };
  assert(1 >= 0 && 1 < b.cap);
  b.data[1] = 99;
  int32_t x = (assert(1 >= 0 && 1 < b.len), b.data[1]);
  printf("%d\n", x);
  return 0;
}
