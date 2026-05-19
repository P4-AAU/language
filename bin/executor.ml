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
    let _env = Typecheck.check_program ast in
    let c_code = Codegen.compile ast in
    let c_file = "output.c" in
    let oc = open_out c_file in
    output_string oc c_code;
    close_out oc;
    let exe_file = "output_program" in
    let compile_cmd = Printf.sprintf "gcc %s -o %s" c_file exe_file in
    let compile_status = Sys.command compile_cmd in
    if compile_status <> 0
    then (
      Printf.eprintf "C compilation failed\n";
      exit 1);
    let run_status = Sys.command ("./" ^ exe_file) in
    exit run_status
  with
  | Failure msg ->
    Printf.eprintf "Type error: %s\n" msg;
    exit 1
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
