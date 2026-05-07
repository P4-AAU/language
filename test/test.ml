open P4_project

let parse str =
  let lexbuf = Lexing.from_string str in
  Parser.file Lexer.token lexbuf
;;

let test_valid () =
  match parse "{ x = 5; }" with
  | _ -> print_endline "OK: { x = 5; }"
  | exception exn ->
      print_endline ("Unexpected error: " ^ Printexc.to_string exn)
;;

let test_invalid () =
  match parse "{ x === 5; }" with
  | _ -> print_endline "FAIL: should have errored"
  | exception exn ->
      print_endline ("Rejected as expected: " ^ Printexc.to_string exn)
;;

let test_buffer_declare () =
  match parse "{ buffer buf<int32, 3> = [1, 2, 3]; }" with
  | _ -> print_endline "OK: buffer declaration with init values"
  | exception exn ->
      print_endline ("FAIL: " ^ Printexc.to_string exn)
;;

let test_buffer_empty () =
  match parse "{ buffer buf<int32, 10> = []; }" with
  | _ -> print_endline "OK: empty buffer declaration"
  | exception exn ->
      print_endline ("FAIL: " ^ Printexc.to_string exn)
;;

let test_bufwrite () =
  match parse "{ buffer buf<int32, 3> = []; bufwrite(buf, 42); }" with
  | _ -> print_endline "OK: bufwrite"
  | exception exn ->
      print_endline ("FAIL: " ^ Printexc.to_string exn)
;;

let test_bufread () =
  match parse "{ buffer buf<int32, 3> = [1, 2, 3]; define x of int32 = bufread(buf, 0); }" with
  | _ -> print_endline "OK: bufread"
  | exception exn ->
      print_endline ("FAIL: " ^ Printexc.to_string exn)
;;

let test_buflen () =
  match parse "{ buffer buf<int32, 3> = [1, 2]; define n of int32 = buflen(buf); }" with
  | _ -> print_endline "OK: buflen"
  | exception exn ->
      print_endline ("FAIL: " ^ Printexc.to_string exn)
;;

let () =
  test_valid ();
  test_invalid ();
  test_buffer_declare ();
  test_buffer_empty ();
  test_bufwrite ();
  test_bufread ();
  test_buflen ()
;;