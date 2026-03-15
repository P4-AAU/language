(* Abstract Syntax of our terribly designed language. *)

(* Identifiers *)
type location = Lexing.position * Lexing.position

type ident =
  { loc : location
  ; id : string
  }

(* Unary Operators*)
type unop =
  | Uneg (* -e *)
  | Unot (* not e *)

(* Binary Operators*)
type binop =
  | Badd
  | Bsub
  | Bmul
  | Bdiv
  | Bmod
  | Beq
  | Bneq
  | Blt (* less than *)
  | Ble (* Less or equal*)
  | Bgt (* Greater than*)
  | Bge (* Greater than or equal*)
  | Band
  | Bor

type constant =
  | Cbool of bool
  | Cint8 of int
  | Cint16 of int
  | Cint32 of int
  | Cint64 of int
  | Cuint8 of int
  | Cuint16 of int
  | Cuint32 of int
  | Cuint64 of int
  | Cfloat16 of float
  | Cfloat32 of float
  | Cfloat64 of float
  | Cchar of char
  | Cstring of string
  | Carray of constant list

type expr =
  | Ecst of constant (* constant*)
  | Eunop of unop * expr (* Unary operation*)
  | Ebinop of binop * expr * expr
  | Eident of ident (* Variable *)
  | Earray of expr list
  | Eindex of expr * expr (* a[i]*)
  | Elen of expr

type pattern =
  | Pdefault (* default eller _ alt efter hvad vi aftaler*)
  | Pconst of constant
  | Pvar of ident

type stmt =
  | Sif of expr * stmt * stmt (* if expr then stmt else stmt*)
  | Sassign of ident * expr (* ident = expr *)
  | Sblock of stmt list (* list of statements *)
  | Sprint of expr list (* print a list of expressions*)
  | Swhile of expr * stmt
  | Sfor of stmt * expr * stmt * stmt (*assignment; condition; increment {body}*)
  | Smatch of expr * (pattern * stmt) list (* match expression*)
  | Sindexset of expr * expr * expr

type file = stmt
