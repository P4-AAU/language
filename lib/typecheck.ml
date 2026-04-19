(*open Ast*)
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

  (*Checks that Cint expr is compatible with the type, or that other expressions are equal to the type*)
let types_compatible ty expr te =
  match expr with
  | Ecst (Cint _) -> is_int_type ty
  | _ -> te = ty

  (*Checks that the value in expr fits into the variable type*)
let check_size ty expr =
  match ty, expr with
  | Tint8,   Ecst (Cint n) ->
      if n < -128 || n > 127 then
        type_error (Printf.sprintf
          "value %d does not fit in int8 (range -128 to 127)" n)
  | Tint16,  Ecst (Cint n) ->
      if n < -32768 || n > 32767 then
        type_error (Printf.sprintf
          "value %d does not fit in int16 (range -32768 to 32767)" n)
  | Tint32,  Ecst (Cint n) ->
      if n < -2147483648 || n > 2147483647 then
        type_error (Printf.sprintf
          "value %d does not fit in int32" n)
  | Tint64,  Ecst (Cint n) ->
      if n < -4611686018427387904 || n > 4611686018427387903 then
        type_error (Printf.sprintf
          "value %d does not fit in int64" n)
  | Tuint8,  Ecst (Cint n) ->
      if n < 0 || n > 255 then
        type_error (Printf.sprintf
          "value %d does not fit in uint8 (range 0 to 255)" n)
  | Tuint16, Ecst (Cint n) ->
      if n < 0 || n > 65535 then
        type_error (Printf.sprintf
          "value %d does not fit in uint16 (range 0 to 65535)" n)
  | Tuint32, Ecst (Cint n) ->
      if n < 0 || n > 4294967295 then
        type_error (Printf.sprintf
          "value %d does not fit in uint32 (range 0 to 4294967295)" n)
  | Tuint64, Ecst (Cint n) ->
      if n < 0 then
        type_error (Printf.sprintf
          "value %d does not fit in uint64 (must be positive)" n)
  | _ -> ()

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
    begin match (op, t) with
    | (Uneg, t) when is_int_type t -> t
    | (Unot, t) when t = Tbool -> Tbool
    | _ -> type_error "invalid type"
    end

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

  (*| Earray ()*)

(*Checks that the right types of expressions are used in statements and updates environment*)
and check_stmt env stmt = 
  match stmt with
      (* Checks if variable to change exists. Checks if type of variable is the same as type of expr. Checks size of expr vs varaible type*)
  | Sassign (id, expr) ->
    let t =
      try Env.find id.id env
      with Not_found ->
        type_error (Printf.sprintf "Variable %s not found" id.id)
    in
    let te = infer_expr env expr in
    if not (types_compatible t expr te) then
      type_error (Printf.sprintf "type mismatch in assign '%s'" id.id);
    check_size t expr;
    env

    (*Checks if variable is already defined. Checks if type of variable is the same as type of expr. Checks size of expr vs varaible type*)
  | Sdefine (id, ty, expr) ->
    if Env.mem id.id env then
      type_error (Printf.sprintf "Variable %s is already defined" id.id);
    let te = infer_expr env expr in
    if not (types_compatible ty expr te) then
      type_error (Printf.sprintf
        "type mismatch in definition of '%s': expected %s but got %s"
        id.id (show_typ ty) (show_typ te));
    check_size ty expr;
    Env.add id.id ty env
  
    (*Checks that condidion is a expr that returns bool. Recursively goes through the next blocks*)
  | Sif (cond, thn, els) ->
    let tc = infer_expr env cond in
    if te <> Tbool then
      type_error  "Expression does not evaluate to bool"
    ignore (check_stmt env thn); 
    ignore (check_stmt env els); 
    env

    (*Goes through every expression in the block recursively*)
  | Sblock stmts -> 
    List.fold_left check_stmt env stmts
  
    (*Checks if the printable expressions are expressions*)
  | Sprint exprs ->
    List.iter (fun e -> ignore (infer_expr env e)) exprs;
    env

    (*Tjekker kun efter at return værdien er expr. ikke om det stemmer overens med function return type*)
  | Sreturn expr ->
    ignore (infer_expr env expr);
    env


  let check_program stmts =
  List.fold_left check_stmt Env.empty stmts