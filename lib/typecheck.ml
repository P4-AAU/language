
(*Make Env map to keep track of variable types in the environment*)
module Env = Map.Make(String)

let type_error msg = failwith msg

(*Checks and returns the type of expressions*)
let rec infer_expr env expr = 
  match expr with
  | Ecst (Cint _) -> Tint32
  | Ecst (Cbool _) -> Tbool
  | Ecst (Cstring _) -> Tstring

  | Eident id -> 
    (try Env.find id.id env
    with Not_found -> type_error (Printf.sprintf "unknown variable: %s" id.id))
    
  | Ebinop

(*Checks that the right types of expressions are used in statements and updates environment*)
and check_stmt env stmt = 
  match stmt with
  | Sassign 

  let check_program stmts =
  List.fold_left check_stmt Env.empty stmts