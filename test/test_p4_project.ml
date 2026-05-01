open Alcotest
open P4_project.Ast

(* ===================================================================
   AST CONSTRUCTION TESTS
   =================================================================== *)

let dummy_loc =
  let pos = Lexing.dummy_pos in
  (pos, pos)

let make_ident id = { loc = dummy_loc; id }

let test_ident_name () =
  let i = make_ident "x" in
  check string "ident name" "x" i.id

let test_constant_bool () =
  let c = Cbool true in
  check bool "constant bool" true (c = Cbool true)

let test_constant_int () =
  let c = Cint32 42 in
  check bool "constant int32" true (c = Cint32 42)

let test_expr_const () =
  let e = Ecst (Cint32 1) in
  check bool "expr const" true (e = Ecst (Cint32 1))

let test_expr_binop () =
  let e = Ebinop (Badd, Ecst (Cint32 1), Ecst (Cint32 2)) in
  check bool "expr binop add" true (e = Ebinop (Badd, Ecst (Cint32 1), Ecst (Cint32 2)))

let test_expr_unop () =
  let e = Eunop (Uneg, Ecst (Cint32 5)) in
  check bool "expr unop neg" true (e = Eunop (Uneg, Ecst (Cint32 5)))

let test_stmt_assign () =
  let id = make_ident "y" in
  let s = Sassign (id, Ecst (Cbool false)) in
  (match s with
   | Sassign (i, Ecst (Cbool false)) -> check string "assign ident" "y" i.id
   | _ -> fail "expected Sassign")

let test_stmt_block () =
  let s = Sblock [] in
  check bool "empty block" true (s = Sblock [])

let test_pattern_default () =
  check bool "pattern default" true (Pdefault = Pdefault)

let test_pattern_const () =
  check bool "pattern const" true (Pconst (Cint32 0) = Pconst (Cint32 0))

(* ===================================================================
   SUITES
   =================================================================== *)

let ident_tests =
  [ test_case "name" `Quick test_ident_name ]

let constant_tests =
  [ test_case "bool" `Quick test_constant_bool
  ; test_case "int32" `Quick test_constant_int ]

let expr_tests =
  [ test_case "const" `Quick test_expr_const
  ; test_case "binop add" `Quick test_expr_binop
  ; test_case "unop neg" `Quick test_expr_unop ]

let stmt_tests =
  [ test_case "assign" `Quick test_stmt_assign
  ; test_case "empty block" `Quick test_stmt_block ]

let pattern_tests =
  [ test_case "default" `Quick test_pattern_default
  ; test_case "const" `Quick test_pattern_const ]

let () =
  run "p4-project"
    [ ("Ident", ident_tests)
    ; ("Constant", constant_tests)
    ; ("Expr", expr_tests)
    ; ("Stmt", stmt_tests)
    ; ("Pattern", pattern_tests) ]
