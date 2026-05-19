#include <stdio.h>
#include <stdint.h>
#include <math.h>
#include <assert.h>

int main(void)
{
  struct { int32_t data[5]; int32_t len; int32_t cap; } my_buffer = { {0}, 0, 5 };
  assert(0 >= 0 && 0 < my_buffer.cap);
  my_buffer.data[0] = 42;
  assert(1 >= 0 && 1 < my_buffer.cap);
  my_buffer.data[1] = 99;
  int32_t x = (assert(0 >= 0 && 0 < my_buffer.cap), my_buffer.data[0]);
  printf("%d\n", x);
  return 0;
}
