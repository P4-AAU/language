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
