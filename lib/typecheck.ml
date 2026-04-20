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
  | Tarray t -> "array of " ^ (show_typ t)
  | Tbuffer (FIFO, t, _) -> "fifo buffer of " ^ (show_typ t)
  | Tbuffer (LIFO, t, _) -> "lifo buffer of " ^ (show_typ t)

let check_pattern_type pattern expected_type =
  match pattern with
  | Pconst (Cint _) when is_int_type expected_type -> ()
  | Pconst (Cbool _) when expected_type = Tbool -> ()
  | Pconst (Cstring _) when expected_type = Tstring -> ()
  | Pconst _ -> type_error "pattern type no match expr type"
  | Pident _ -> ()
  | Pwildcard -> ()

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

  | Earray _ | Eindex _ | Eslice _ | Elength _ ->
    type_error "expression type not implemented"

and check_return_in_stmt env stmt = 
   match stmt with
   | Sreturn expr -> (true, infer_expr env expr)
   | Sblock stmts -> List.fold_left (fun (has_return, return_type) s -> 
   let (stmt_has_return, stmt_has_return_of_type) = check_return_in_stmt env s in 
   match has_return, stmt_has_return with
   | false,true -> (true, stmt_has_return_of_type)
   | true, true when return_type = stmt_has_return_of_type -> (true, return_type)
   | true, true -> type_error "inconsistent return type"
   | _ -> (has_return, return_type)
   ) (false, Tint32) stmts

   | Sif (_,then_body,else_body) ->
   let (then_return, then_return_type) = check_return_in_stmt env then_body in
   let (else_return, else_return_type) = check_return_in_stmt env else_body in

   (match then_return, else_return with
      | true, true when then_return_type = else_return_type -> (true, then_return_type)
      | true, true -> type_error "blocks of different types"
      | true,false | false,true| false, false -> (false,Tint32))

   (* Other statements don't contain returns *)
   | Sassign _| Sdefine _ | Sprint _ | Sfor _ | Smatch _
   | Sbuffer _ | Sdelete _ | Sinput _ | Sforrange _ | Sassign_index _| Sfunc _ ->
        (false, Tint32)
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
    if tc <> Tbool then
         type_error  "Expression does not evaluate to bool";
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
  | Sfunc (func_name, func_type, params_list, func_body) -> 
      if Env.mem func_name.id env then 
         type_error (Printf.sprintf "Function %s already defined: " func_name.id);
      
   let function_scope = List.fold_left (fun local_env (param_name,param_type)->
         if Env.mem param_name.id local_env then type_error ( Printf.sprintf "Variable already defined inside function %s" param_name.id);
         Env.add param_name.id param_type local_env
      ) env params_list in
   let (has_return, actual_return_type) = check_return_in_stmt function_scope func_body in

   if not has_return then 
         type_error (Printf.sprintf "function %s has no return" func_name.id);
   if actual_return_type <> func_type then
         type_error "return type does not match function type";
   
   Env.add func_name.id func_type env;

  | Sforrange (iterator_name, start,stop, body) ->
      if Env.mem iterator_name.id env then 
         type_error "iterator name already defined";
      let start_is_Tint = is_int_type (infer_expr env start) in 
      let stop_is_Tint = is_int_type (infer_expr  env stop) in 
      if not (start_is_Tint && stop_is_Tint) then  
         type_error "range is undefined in for loop - input not of type int";
      ignore (check_stmt  env body);
      env
  | Sfor (iter_name, input, body) ->
      if Env.mem iter_name.id env then 
         type_error "iterator name already defined";
      let input_type = infer_expr env input in
      let iterator_type = match input_type with
      | Tarray elm_type ->  elm_type
      | Tstring -> Tstring 
      | Tbuffer (_,elm_type,_) -> elm_type
      |_ -> type_error "input not iterable type"
      in
      let loop_env = Env.add iter_name.id iterator_type env in 
      ignore (check_stmt loop_env body);
      env

  | Smatch (expr, cases) ->
    let expr_type = infer_expr env expr in
    List.iter (fun (pattern, stmt) ->
        check_pattern_type pattern expr_type;
        ignore (check_stmt env stmt)
    ) cases;
    env  
      |Sassign_index _| Sbuffer _ | Sdelete _ | Sinput _ -> 
         type_error "expression type not implemented"


  let check_program stmts =
  List.fold_left check_stmt Env.empty stmts
