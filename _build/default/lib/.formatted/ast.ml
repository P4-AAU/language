type expr =
  | Int of int
  | Var of string
  | Add of expr * expr
  | Let of string * expr * expr
