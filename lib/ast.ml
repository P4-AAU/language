(* Abstract Syntax of our language - minimal pipeline version *)

type location = Lexing.position * Lexing.position

type ident =
  { loc : location
  ; id : string
  }

type buffer_kind =
  | FIFO
  | LIFO

  
type unop =
  | Uneg
  | Unot
  | Usqrt

type binop =
  | Badd
  | Bsub
  | Bmul
  | Bdiv
  | Bmod
  | Bpow
  | Beq
  | Bneq
  | Blt
  | Ble
  | Bgt
  | Bge
  | Band
  | Bor

type constant =
  | Cint of int
  | Cbool of bool
  | Cstring of string

type pattern =
  | Pconst of constant
  | Pident of ident
  | Pwildcard

type expr =
  { expr_loc : location
  ; expr_node : expr_node
  }

and expr_node =
  | Ecst of constant
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr
  | Eident of ident
  | Earray of expr list
  | Eindex of expr * expr
  | Eslice of expr * expr * expr
  | Elength of expr
  | Ecall of ident * expr list

and typ =
  | Tint8
  | Tint16
  | Tint32
  | Tint64
  | Tuint8
  | Tuint16
  | Tuint32
  | Tuint64
  | Tbool
  | Tstring
  | Tarray of typ
  | Tbuffer of buffer_kind * typ * expr

type stmt =
  | Sif of expr * stmt * stmt
  | Sassign of ident * expr
  | Sblock of stmt list
  | Sprint of expr list
  | Sreturn of expr
  | Sfunc of ident * typ * (ident * typ) list * stmt
  | Sfor of ident * expr * stmt
  | Sdefine of bool * ident * typ * expr
  | Sassign_index of ident * expr * expr
  | Smatch of expr * (pattern * stmt) list
  | Sbuffer of ident * typ * expr * expr list
  | Sdelete of ident
  | Sinput of ident * typ
  | Sforrange of ident * expr * expr * stmt

type file = stmt list
