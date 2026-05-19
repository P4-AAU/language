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

  $ main invalid/bufread_nonbuffer.mylang
  Type error: invalid/bufread_nonbuffer.mylang:3:33: bufread expects a buffer
  [1]

  $ main invalid/bufwrite_nonbuffer.mylang
  Type error: invalid/bufwrite_nonbuffer.mylang:3:14: expected a buffer but got int32
  [1]

  $ main invalid/bufread_wrong_index_type.mylang
  Type error: invalid/bufread_wrong_index_type.mylang:3:44: buffer index must be an integer type
  [1]

  $ main invalid/bufwrite_wrong_type.mylang
  Type error: invalid/bufwrite_wrong_type.mylang:3:25: type mismatch in bufwrite: expected int32 but got bool
  [1]
