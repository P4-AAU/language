Invalid test: define variable of type integer and set it to 256
  $ echo "{ define y of int8 = 256;}" > program.mylang
  $ main program.mylang
  Type error: program.mylang:1:22: value 256 does not fit in int8 (range -128 to 127)
  [1]


  $ main invalid/larger_int.mylang
  Type error: invalid/larger_int.mylang:1:21: value 256 does not fit in int8 (range -128 to 127)
  [1]

  $ main invalid/syntax_error.mylang
  Lexing error: unexpected character: !
  [1]

  $ main invalid/arithmetic.mylang
  Type error: invalid/arithmetic.mylang:3:21: invalid operand types for binary operator
  [1]

  $ main invalid/parametertype.mylang
  Syntax error at invalid/parametertype.mylang:5
  [1]

  $ main invalid/immutable_reassign.mylang
  Type error: invalid/immutable_reassign.mylang:3:5: variable x is immutable
  [1]

  $ main invalid/undefined_variable.mylang
  Type error: invalid/undefined_variable.mylang:2:11: unknown variable: x
  [1]

  $ main invalid/bufwriteinvalid.mylang
  #include <stdio.h>
  #include <stdint.h>
  #include <math.h>
  
  int main(void)
  {
    int32_t buf_data[3];
    int buf_len = 0;
    int32_t dummy = 0;
    dummy = buf_data[buf_len++] = 1;
    dummy = buf_data[buf_len++] = 2;
    dummy = buf_data[buf_len++] = 3;
    dummy = buf_data[buf_len++] = 4;
    dummy = buf_data[buf_len++] = 5;
    return 0;
  }

  $ main invalid/bufreadinvalid.mylang
  Syntax error at invalid/bufreadinvalid.mylang:9
  [1]
