  $ echo "{define x of int8 = 1;}" > program.mylang
  $ main program.mylang
  #include <stdio.h>
  #include <stdint.h>
  #include <math.h>
  
  int main(void)
  {
    static int8_t x = 1;
    return 0;
  }

  $ main valid/arithmetic.mylang
  Type error: valid/arithmetic.mylang:3:21: invalid operand types for binary operator
  [1]


