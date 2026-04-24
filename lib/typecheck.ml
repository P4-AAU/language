open Ast

(*Make Env map to keep track of variable types in the environment*)
module Env = Map.Make (String)

type env_entry =
  | Var of typ
  | Func of typ list * typ

let type_error (loc : location) msg =
  let pos, _ = loc in
  failwith
    (Printf.sprintf
       "%s:%d:%d: %s"
       pos.pos_fname
       pos.pos_lnum
       (pos.pos_cnum - pos.pos_bol + 1)
       msg)
;;

(* gruppering af integers, for at kunne lave "when same_numeric_type" *)
let is_int_type = function
  | Tint8 | Tint16 | Tint32 | Tint64 | Tuint8 | Tuint16 | Tuint32 | Tuint64 -> true
  | _ -> false
;;

(* Logik: Her vil Tint8 = Tint16 give error - kan evt. laves om til at acceptere det *)
let same_numeric_type t1 t2 = is_int_type t1 && t1 = t2

(* bruges til at definere hvilke typer der må sammenlignes i Beq og Bneq *)
let is_comparable_type = function
  | Tbool | Tstring -> true
  | t when is_int_type t -> true
  | _ -> false
;;

let rec show_typ = function
  | Tint8 -> "int8"
  | Tint16 -> "int16"
  | Tint32 -> "int32"
  | Tint64 -> "int64"
  | Tuint8 -> "uint8"
  | Tuint16 -> "uint16"
  | Tuint32 -> "uint32"
  | Tuint64 -> "uint64"
  | Tbool -> "bool"
  | Tstring -> "string"
  | Tarray t -> "array of " ^ show_typ t
  | Tbuffer (FIFO, t, _) -> "fifo buffer of " ^ show_typ t
  | Tbuffer (LIFO, t, _) -> "lifo buffer of " ^ show_typ t
;;

let check_pattern_type loc pattern expected_type =
  match pattern with
  | Pconst (Cint _) when is_int_type expected_type -> ()
  | Pconst (Cbool _) when expected_type = Tbool -> ()
  | Pconst (Cstring _) when expected_type = Tstring -> ()
  | Pconst _ -> type_error loc "pattern type no match expr type"
  | Pident _ -> ()
  | Pwildcard -> ()
;;

(*Checks that Cint expr is compatible with the type, or that other expressions are equal to the type*)
let types_compatible ty expr te =
  match expr.expr_node with
  | Ecst (Cint _) -> is_int_type ty
  | _ -> te = ty
;;

(*Checks that the value in expr fits into the variable type*)
let check_size ty expr =
  match ty, expr.expr_node with
  | Tint8, Ecst (Cint n) ->
    if n < -128 || n > 127
    then
      type_error
        expr.expr_loc
        (Printf.sprintf "value %d does not fit in int8 (range -128 to 127)" n)
  | Tint16, Ecst (Cint n) ->
    if n < -32768 || n > 32767
    then
      type_error
        expr.expr_loc
        (Printf.sprintf "value %d does not fit in int16 (range -32768 to 32767)" n)
  | Tint32, Ecst (Cint n) ->
    if n < -2147483648 || n > 2147483647
    then type_error expr.expr_loc (Printf.sprintf "value %d does not fit in int32" n)
  | Tint64, Ecst (Cint n) ->
    if n < -4611686018427387904 || n > 4611686018427387903
    then type_error expr.expr_loc (Printf.sprintf "value %d does not fit in int64" n)
  | Tuint8, Ecst (Cint n) ->
    if n < 0 || n > 255
    then
      type_error
        expr.expr_loc
        (Printf.sprintf "value %d does not fit in uint8 (range 0 to 255)" n)
  | Tuint16, Ecst (Cint n) ->
    if n < 0 || n > 65535
    then
      type_error
        expr.expr_loc
        (Printf.sprintf "value %d does not fit in uint16 (range 0 to 65535)" n)
  | Tuint32, Ecst (Cint n) ->
    if n < 0 || n > 4294967295
    then
      type_error
        expr.expr_loc
        (Printf.sprintf "value %d does not fit in uint32 (range 0 to 4294967295)" n)
  | Tuint64, Ecst (Cint n) ->
    if n < 0
    then
      type_error
        expr.expr_loc
        (Printf.sprintf "value %d does not fit in uint64 (must be positive)" n)
  | _ -> ()
;;

(*Checks and returns the type of expressions*)
let rec infer_expr env expr =
  match expr.expr_node with
  | Ecst (Cint _) -> Tint32
  | Ecst (Cbool _) -> Tbool
  | Ecst (Cstring _) -> Tstring
  | Eident id ->
    (match
       try Some (Env.find id.id env) with
       | Not_found -> None
     with
     | None -> type_error id.loc (Printf.sprintf "unknown variable: %s" id.id)
     | Some (Var t) -> t
     | Some (Func _) ->
       type_error id.loc (Printf.sprintf "%s is a function, not a variable" id.id))
  | Eunop (op, e) ->
    let t = infer_expr env e in
    (match op, t with
     | Uneg, t when is_int_type t -> t
     | Unot, t when t = Tbool -> Tbool
     | _ -> type_error expr.expr_loc "invalid type")
  | Ebinop (op, e1, e2) ->
    let t1 = infer_expr env e1 in
    let t2 = infer_expr env e2 in
    (match op, t1, t2 with
     | (Badd | Bsub | Bmul | Bdiv | Bmod | Bpow), t1, t2 when same_numeric_type t1 t2 ->
       t1
     | (Blt | Ble | Bgt | Bge), t1, t2 when same_numeric_type t1 t2 -> Tbool
     | (Beq | Bneq), t1, t2 when t1 = t2 && is_comparable_type t1 -> Tbool
     | (Band | Bor), Tbool, Tbool -> Tbool
     | _ -> type_error expr.expr_loc "invalid operand types for binary operator")
  | Ecall (id, args) ->
    (match
       try Some (Env.find id.id env) with
       | Not_found -> None
     with
     | None -> type_error id.loc (Printf.sprintf "unknown function: %s" id.id)
     | Some (Var _) ->
       type_error id.loc (Printf.sprintf "%s is a variable, not a function" id.id)
     | Some (Func (param_types, ret_type)) ->
       if List.length args <> List.length param_types
       then
         type_error
           id.loc
           (Printf.sprintf
              "function %s expects %d arguments but got %d"
              id.id
              (List.length param_types)
              (List.length args));
       List.iter2
         (fun param_type arg ->
            let arg_type = infer_expr env arg in
            if not (types_compatible param_type arg arg_type)
            then type_error arg.expr_loc "argument type does not match parameter type")
         param_types
         args;
       ret_type)
  | Earray _ | Eindex _ | Eslice _ | Elength _ ->
    type_error expr.expr_loc "expression type not implemented"

and check_return_in_stmt env loc stmt =
  match stmt with
  | Sreturn expr -> true, infer_expr env expr
  | Sblock stmts ->
    List.fold_left
      (fun (has_return, return_type) s ->
         let stmt_has_return, stmt_return_type = check_return_in_stmt env loc s in
         match has_return, stmt_has_return with
         | false, true -> true, stmt_return_type
         | true, true when return_type = stmt_return_type -> true, return_type
         | true, true -> type_error loc "inconsistent return type"
         | _ -> has_return, return_type)
      (false, Tint32)
      stmts
  | Sif (cond, then_body, else_body) ->
    let then_return, then_return_type = check_return_in_stmt env loc then_body in
    let else_return, else_return_type = check_return_in_stmt env loc else_body in
    (match then_return, else_return with
     | true, true when then_return_type = else_return_type -> true, then_return_type
     | true, true -> type_error cond.expr_loc "blocks of different types"
     | _ -> false, Tint32)
  (* Other statements don't contain returns *)
  | Sassign _
  | Sdefine _
  | Sprint _
  | Sfor _
  | Smatch _
  | Sbuffer _
  | Sdelete _
  | Sinput _
  | Sforrange _
  | Sassign_index _
  | Sfunc _ -> false, Tint32

(*Checks that the right types of expressions are used in statements and updates environment*)
and check_stmt env stmt =
  match stmt with
  (* Checks if variable to change exists. Checks if type of variable is the same as type of expr. Checks size of expr vs varaible type*)
  | Sassign (id, expr) ->
    let t =
      match
        try Some (Env.find id.id env) with
        | Not_found -> None
      with
      | None -> type_error id.loc (Printf.sprintf "Variable %s not found" id.id)
      | Some (Func _) ->
        type_error id.loc (Printf.sprintf "%s is a function, not a variable" id.id)
      | Some (Var t) -> t
    in
    let te = infer_expr env expr in
    if not (types_compatible t expr te)
    then type_error expr.expr_loc (Printf.sprintf "type mismatch in assign '%s'" id.id);
    check_size t expr;
    env
    (*Checks if variable is already defined. Checks if type of variable is the same as type of expr. Checks size of expr vs varaible type*)
  | Sdefine (id, ty, expr) ->
    if Env.mem id.id env
    then type_error id.loc (Printf.sprintf "Variable %s is already defined" id.id);
    let te = infer_expr env expr in
    if not (types_compatible ty expr te)
    then
      type_error
        id.loc
        (Printf.sprintf
           "type mismatch in definition of '%s': expected %s but got %s"
           id.id
           (show_typ ty)
           (show_typ te));
    check_size ty expr;
    Env.add id.id (Var ty) env
    (*Checks that condidion is a expr that returns bool. Recursively goes through the next blocks*)
  | Sif (cond, thn, els) ->
    let tc = infer_expr env cond in
    if tc <> Tbool then type_error cond.expr_loc "expression does not evaluate to bool";
    ignore (check_stmt env thn);
    ignore (check_stmt env els);
    env
    (*Goes through every expression in the block recursively*)
  | Sblock stmts ->
    List.fold_left check_stmt env stmts
    (*Checks if the printable expressions are expressions*)
  | Sprint exprs ->
    List.iter (fun e -> ignore (infer_expr env e)) exprs;
    env (*Checks if return value is an expression*)
  | Sreturn expr ->
    ignore (infer_expr env expr);
    env
  | Sfunc (func_name, func_type, params_list, func_body) ->
    if Env.mem func_name.id env
    then
      type_error func_name.loc (Printf.sprintf "Function %s already defined" func_name.id);
    let function_scope =
      List.fold_left
        (fun local_env (param_name, param_type) ->
           if Env.mem param_name.id local_env
           then
             type_error
               param_name.loc
               (Printf.sprintf "parameter %s already defined" param_name.id);
           Env.add param_name.id (Var param_type) local_env)
        env
        params_list
    in
    let checked_scope = check_stmt function_scope func_body in
    let has_return, actual_return_type =
      check_return_in_stmt checked_scope func_name.loc func_body
    in
    if not has_return
    then
      type_error func_name.loc (Printf.sprintf "function %s has no return" func_name.id);
    if actual_return_type <> func_type
    then type_error func_name.loc "return type does not match function type";
    Env.add
      func_name.id
      (Func (List.map snd params_list, func_type))
      env (* Adder function som: {navn, list af param_types, return_type}*)
  | Sforrange (iterator_name, start, stop, body) ->
    if Env.mem iterator_name.id env
    then type_error iterator_name.loc "iterator name already defined";
    let start_is_int = is_int_type (infer_expr env start) in
    let stop_is_int = is_int_type (infer_expr env stop) in
    if not start_is_int
    then type_error start.expr_loc "range bounds must be integer types";
    if not stop_is_int then type_error stop.expr_loc "range bounds must be integer types";
    ignore (check_stmt env body);
    env
  | Sfor (iter_name, input, body) ->
    if Env.mem iter_name.id env
    then type_error iter_name.loc "iterator name already defined";
    let input_type = infer_expr env input in
    let iterator_type =
      match input_type with
      | Tarray elm_type -> elm_type
      | Tstring -> Tstring
      | Tbuffer (_, elm_type, _) -> elm_type
      | _ -> type_error input.expr_loc "input not iterable type"
    in
    let loop_env = Env.add iter_name.id (Var iterator_type) env in
    ignore (check_stmt loop_env body);
    env
  | Smatch (expr, cases) ->
    let expr_type = infer_expr env expr in
    List.iter
      (fun (pattern, stmt) ->
         check_pattern_type expr.expr_loc pattern expr_type;
         ignore (check_stmt env stmt))
      cases;
    env
  | Sassign_index (id, _, _) -> type_error id.loc "assign index not implemented"
  | Sbuffer (name, _, _, _) -> type_error name.loc "buffer not implemented"
  | Sdelete id -> type_error id.loc "delete not implemented"
  | Sinput (id, _) -> type_error id.loc "input not implemented"
;;

let check_program stmts = List.fold_left check_stmt Env.empty stmts
