open P4_project

let () =
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p
  <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  try
    let ast = Parser.file Lexer.token lexbuf in
    close_in ic;
    let c_code = Codegen.compile ast in
    print_string c_code
  with
  | Lexer.Lexing_error msg ->
    Printf.eprintf "Lexing error: %s\n" msg;
    exit 1
  | Parser.Error ->
    Printf.eprintf
      "Syntax error at %s:%d\n"
      filename
      lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum;
    exit 1
;;
