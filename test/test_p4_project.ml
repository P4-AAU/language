open P4_project

let parse str =
  let lexbuf = Lexing.from_string str in
  Parser.file Lexer.token lexbuf
;;

let test_valid () =
  match parse "x = 5" with
  | _ -> print_endline "OK: x = 5"
;;

let test_invalid () =
  match parse "x === 5" with
  | _ -> print_endline "FAIL: should have errored"
  | exception Parser.Error -> print_endline "x === 5 rejected"
;;

let () =
  test_valid ();
  test_invalid ()
;;
