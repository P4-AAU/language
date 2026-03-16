(* Abstract Syntax of our terribly designed language. *)

(* ===================================================================
   LOCATIONS AND IDENTIFIERS: 
   - location is a span in the source file. i.e lines in error messages
   - identifier is a an object with a location and a name
   =================================================================== *)
type location = Lexing.position * Lexing.position

type ident =
  { loc : location
  ; id : string
  }

(* ===================================================================
   OPERATORS
   =================================================================== *)

(** Unary operators. *)
type unop =
  | Uneg
  | Unot

(** Binary operators. *)
type binop =
  | Badd (**        Addition               *)
  | Bsub (**        Subtraction            *)
  | Bmul (**        Multiplication         *)
  | Bdiv (**        Division               *)
  | Bmod (**        Modulo:                *)
  | Beq (**         Equality:             *)
  | Bneq (**        Inequality:            *)
  | Blt (**         Less than:            *)
  | Ble (**         Less than or equal    *)
  | Bgt (**         Greater than          *)
  | Bge (**         Greater than or equal *)
  | Band (**        and                    *)
  | Bor (**         or                    *)

(* ===================================================================
   CONSTANTS
   =================================================================== *)

(** Literal constant values supported by the language.*)
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
  | Cbuffer of constant list
  (** Buffer literal. Unlike arrays, buffers are
                          bounds-checked at runtime. e.g. [buf{1, 2, 3}] *)

(* ===================================================================
   TYPES AND BUFFERS
   =================================================================== *)

(** Runtime representation of a buffer.
    Holds the element types, the maximum capacity, and the current
    pointer position used for bounds checking.*)
type buffer =
  { data : typ list
  ; size : int
  ; ptr : int
  }

(** Types in the language. Used by the typechecker to verify that
    expressions are used correctly, and by [Tbuffer.data] to track
    what kind of value is stored at each position in a buffer. *)
and typ =
  | Tbool
  | Tint8
  | Tint16
  | Tint32
  | Tint64
  | Tuint8
  | Tuint16
  | Tuint32
  | Tuint64
  | Tfloat16
  | Tfloat32
  | Tfloat64
  | Tchar
  | Tstring
  | Tarray of typ
  | Tbuffer of buffer
  (** Buffer type with element type and size,
                          subject to runtime bounds checking *)

(* ===================================================================
   EXPRESSIONS
   =================================================================== *)

type expr =
  | Ecst of constant
  | Eunop of unop * expr (** Unary operation*)
  | Ebinop of binop * expr * expr (** Binary operation*)
  | Eident of ident (** Variable reference *)
  | Earray of expr list (** Array literal *)
  | Eindex of expr * expr (** Array index access [a[i]]*)
  | Elen of expr (**Length of an array *)
  | Ebuffer of expr list (** Buffer literal*)
  | Ebufindex of expr * expr
  (** Buffer index access [buf[i]],
                                   triggers runtime bounds check *)
  | Ebuflen of expr (** Length of a buffer *)

(* ===================================================================
   PATTERNS
   =================================================================== *)

(** Patterns used in [match] expressions. *)
type pattern =
  | Pdefault (** default case *)
  | Pconst of constant (** Constant pattern*)
  | Pvar of ident (** Variable pattern *)

(* ===================================================================
   STATEMENTS
   =================================================================== *)
type stmt =
  | Sif of expr * stmt * stmt (** Conditional: [if expr then stmt else stmt] *)
  | Sassign of ident * expr (** Assignment: [ident = expr] *)
  | Sblock of stmt list (** Sequence of statements *)
  | Sprint of expr list (** Print a list of expressions *)
  | Swhile of expr * stmt (** While loop: [while expr do stmt] *)
  | Sfor of stmt * expr * stmt * stmt (** For loop: [for (init; cond; incr) { body }] *)
  | Smatch of expr * (pattern * stmt) list
  (** Match expression: [match expr { pattern => stmt }] *)
  | Sindexset of expr * expr * expr
  (** Array element write: [a[i] = e], no bounds check *)
  | Sbufset of expr * expr * expr
  (** Buffer element write: [buf[i] = e],
                                   triggers runtime bounds check *)
  | Sbuferror
  (** Runtime buffer overflow error.
                                   Emitted by code generation when [ptr >= size]. *)

(** A complete program is a single top-level statement.
    Typically this will be an [Sblock] containing the program body. *)
type file = stmt
