open Alcotest
open P4_project.Ast
open P4_project.Typecheck
open P4_project.Parser

module Env = Map.Make (String)

let dummy_loc =
  let pos = Lexing.dummy_pos in
  (pos, pos)

let make_ident id = { loc = dummy_loc; id }
let make_expr node = { expr_loc = dummy_loc; expr_node = node }


(*TYPECHECKER UNIT TESTS --------------------------*)
(* runs f () and fails the test if no type error is raised *)
let assert_type_error f =
  match f () with
  | _ -> fail "expected a type error but typechecker accepted it"
  | exception Failure _ -> ()

(* int literal Cint infers to Tint32 *)
let test_typecheck_infer_int_literal () =
  let e = make_expr (Ecst (Cint 42)) in
  match infer_expr Env.empty e with
  | Tint32 -> ()
  | t -> fail ("Expected Tint32, got " ^ show_typ t)

(* variable lookup returns the type it was declared with *)
let test_typecheck_infer_ident () =
  let id = make_ident "x" in
  let env = Env.add "x" (Var (Tint32, false)) Env.empty in
  let e = make_expr (Eident id) in
  match infer_expr env e with
  | Tint32 -> ()
  | t -> fail ("Expected Tint32, got " ^ show_typ t)

(* arithmetic on same type returns that type: int32 + int32 = int32 *)
let test_binop_add_returns_input_type () =
  let lhs = make_expr (Ecst (Cint 1)) in
  let rhs = make_expr (Ecst (Cint 2)) in
  let e = make_expr (Ebinop (Badd, lhs, rhs)) in
  match infer_expr Env.empty e with
  | Tint32 -> ()
  | t -> fail ("Expected Tint32, got " ^ show_typ t)

(* comparison on same type returns bool: int32 < int32 = bool *)
let test_binop_comparison_returns_bool () =
  let lhs = make_expr (Ecst (Cint 1)) in
  let rhs = make_expr (Ecst (Cint 2)) in
  let e = make_expr (Ebinop (Blt, lhs, rhs)) in
  match infer_expr Env.empty e with
  | Tbool -> ()
  | t -> fail ("Expected Tbool, got " ^ show_typ t)

(* int8 + int32 must be rejected — numeric types are not implicitly widened *)
let test_rejects_mixed_numeric_types () =
  let env = Env.add "x" (Var (Tint8, false)) Env.empty in
  let lhs = make_expr (Eident (make_ident "x")) in
  let rhs = make_expr (Ecst (Cint 1)) in
  let e = make_expr (Ebinop (Badd, lhs, rhs)) in
  assert_type_error (fun () -> ignore (infer_expr env e))

(* 256 does not fit in int8 (range -128 to 127) *)
let test_rejects_int_out_of_range () =
  let stmt = Sdefine (false, make_ident "x", Tint8, make_expr (Ecst (Cint 256))) in
  assert_type_error (fun () -> ignore (check_stmt Env.empty stmt))

(* referencing a variable not in scope must be rejected *)
let test_rejects_undefined_variable () =
  let e = make_expr (Eident (make_ident "undefined")) in
  assert_type_error (fun () -> ignore (infer_expr Env.empty e))

(* calling f(x) where f expects bool but x is int32 must be rejected *)
let test_rejects_wrong_argument_type () =
  let env = Env.add "f" (Func ([ Tbool ], Tint32)) Env.empty in
  let env = Env.add "x" (Var (Tint32, false)) env in
  let arg = make_expr (Eident (make_ident "x")) in
  let e = make_expr (Ecall (make_ident "f", [ arg ])) in
  assert_type_error (fun () -> ignore (infer_expr env e))

let typecheck_tests =
  [ test_case "infer int literal" `Quick test_typecheck_infer_int_literal
  ; test_case "infer ident from env" `Quick test_typecheck_infer_ident
  ; test_case "binop add returns input type" `Quick test_binop_add_returns_input_type
  ; test_case "binop comparison returns bool" `Quick test_binop_comparison_returns_bool
  ; test_case "rejects mixed numeric types" `Quick test_rejects_mixed_numeric_types
  ; test_case "rejects int out of range" `Quick test_rejects_int_out_of_range
  ; test_case "rejects undefined variable" `Quick test_rejects_undefined_variable
  ; test_case "rejects wrong argument type" `Quick test_rejects_wrong_argument_type ]


(*PARSER/LEXER/AST INTEGRATION TESTS --------------------------*)
let parse str =
  let lexbuf = Lexing.from_string str in
  P4_project.Parser.file P4_project.Lexer.token lexbuf

(* define x of int8 = 1 should parse into a Sdefine with name x and type Tint8 *)
let test_parse_define () =
  match parse "{ define x of int8 = 1; }" with
  | [ Sdefine (_, id, Tint8, _) ] ->
    check string "variable name" "x" id.id
  | _ -> fail "expected Sdefine with Tint8"

(* invalid syntax should raise Parser.Error *)
let test_parse_rejects_invalid_syntax () =
  match parse "{ x === 5; }" with
  | _ -> fail "expected a parse error"
  | exception Error -> ()

let parser_tests =
  [ test_case "parse define" `Quick test_parse_define
  ; test_case "rejects invalid syntax" `Quick test_parse_rejects_invalid_syntax ]

let () =
  run "p4-project"
    [ ("Typechecker", typecheck_tests)
    ; ("Parser", parser_tests) ]
