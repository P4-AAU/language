open Ast
(*Make Env map to keep track of variable types in the environment*)
module Env = Map.Make(String)

let type_error msg = failwith msg

(* gruppering af integers, for at kunne lave "when same_numeric_type" *)
let is_int_type = function
  | Tint8 | Tint16 | Tint32 | Tint64
  | Tuint8 | Tuint16 | Tuint32 | Tuint64 -> true
  | _ -> false

(* Logik: Her vil Tint8 = Tint16 give error - kan evt. laves om til at acceptere det *)
let same_numeric_type t1 t2 =
  is_int_type t1 && t1 = t2

(* bruges til at definere hvilke typer der må sammenlignes i Beq og Bneq *)
let is_comparable_type = function
  | Tbool | Tstring -> true
  | t when is_int_type t -> true
  | _ -> false

(*Checks and returns the type of expressions*)
let rec infer_expr env expr = 
  match expr with
  | Ecst (Cint _) -> Tint32
  | Ecst (Cbool _) -> Tbool
  | Ecst (Cstring _) -> Tstring

  | Eident id -> 
    (try Env.find id.id env
    with Not_found -> type_error (Printf.sprintf "unknown variable: %s" id.id))
    
  | Eunop (op, e) ->
    let t = infer_expr env e in
    match (op, t) with
    | (Uneg, t)
    when is_int_type t -> t
    | (Unot, t)
    when t = Tbool -> Tbool
    | _ -> type_error "invalid type"

  | Ebinop (op, e1, e2) ->
    let t1 = infer_expr env e1 in
    let t2 = infer_expr env e2 in
    begin match (op, t1, t2) with
    | ((Badd | Bsub | Bmul | Bdiv | Bmod | Bpow), t1, t2)
    when same_numeric_type t1 t2 -> t1
    | ((Blt | Ble | Bgt | Bge), t1, t2)
    when same_numeric_type t1 t2 -> Tbool
    | ((Beq | Bneq), t1, t2)
    when t1 = t2 && is_comparable_type t1 -> Tbool
    | ((Band | Bor), Tbool, Tbool) -> Tbool
    | _ -> type_error "invalid operand types for binary operator"
    end

  | Earray ()

(*Checks that the right types of expressions are used in statements and updates environment*)
and check_stmt env stmt = 
  match stmt with
  | Sassign 

  let check_program stmts =
  List.fold_left check_stmt Env.empty stmts