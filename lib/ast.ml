(* Abstract Syntax of our language - minimal pipeline version *)

type location = Lexing.position * Lexing.position

type ident =
  { loc : location
  ; id : string
  }

type unop =
  | Uneg
  | Unot

type binop =
  | Badd
  | Bsub
  | Bmul
  | Bdiv
  | Bmod
  | Beq
  | Bneq
  | Blt
  | Ble
  | Bgt
  | Bge
  | Band
  | Bor

type constant =
  | Cbool of bool
  | Cint of int

type expr =
  | Ecst of constant
  | Eunop of unop * expr
  | Ebinop of binop * expr * expr
  | Eident of ident

type stmt =
  | Sif of expr * stmt * stmt
  | Sassign of ident * expr
  | Sblock of stmt list
  | Sprint of expr list
  | Swhile of expr * stmt

type file = stmt
