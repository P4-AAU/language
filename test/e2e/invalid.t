Invalid programs should be rejected with a non-zero exit code and a
diagnostic on stderr. Cases marked [lexer pending] currently exit 0
because the lexer is not yet implemented; update them to [1] once it is.

Empty files are detectable without a lexer and already fail correctly.

  $ p4-project invalid/empty.p4
  Error: 'invalid/empty.p4' is empty
  [1]

A file whose first token is illegal should be rejected by the lexer.
[lexer pending] — currently exits 0 silently.

  $ p4-project invalid/syntax_error.p4
