#include <stdio.h>
#include <stdint.h>
#include <math.h>

int main(void)
{
  int32_t function_plus(int32_t a, int32_t b)  {
    int32_t x = 0;
    return a + b + x;
  }
  int32_t x = 10;
  int32_t y = 20;
  printf("%d\n", function_plus(x, y));
  return 0;
}
