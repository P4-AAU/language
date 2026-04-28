open P4_project

let parse str =
  let lexbuf = Lexing.from_string str in
  Parser.file Lexer.token lexbuf
;;

let test_valid () =
  match parse "{ define mut x of int32 = 32; }" with
  | _ -> print_endline "Pass"
  | exception exn ->
      print_endline ("Unexpected error: " ^ Printexc.to_string exn)
;;

let test_invalid () =
  match parse "{ x === 5; }" with
  | _ -> print_endline "FAIL: should have errored"
  | exception exn ->
      print_endline ("Rejected as expected: " ^ Printexc.to_string exn)
;;

let () =
  test_valid ();
  test_invalid ()
;;