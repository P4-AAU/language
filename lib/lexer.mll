(* Lexical analyzer for our Language. *)
{
 open Lexing
 open Ast
 open Parser


 exception Lexing_error of string


  let id_or_kwg =
   let h = Hashtbl.create 16 in
   List.iter (fun(s, token) -> Hashtbl.add h s token)
   [
     "if",     IF;
     "else",   ELSE;
     "print",  PRINT;
     "and",    AND;
     "or",     OR;
     "not",    NOT;
     "true",   CST (Cbool true);
     "false",  CST (Cbool false);
     "return", RETURN;
     "lengthof", LENGTHOF;
     "match",  MATCH;
     "with",   WITH;
     "to",     TO;
     "delete", DELETE;
     "input",  INPUT;
     "define", DEFINE;
     "of",     OF;
     "int8",   INT8;
     "int16",  INT16;
     "int32",  INT32;
     "int64",  INT64;
     "uint8",  UINT8;
     "uint16", UINT16;
     "uint32", UINT32;
     "uint64", UINT64;
     "bool",   BOOL;
     "string", STRING;
     "array",  ARRAY;
     "buffer", BUFFER;
     "lifo",   LIFO;
     "fifo",   FIFO;
     "sqrt",   SQRT;
     "mut",    MUT;
     "imut",   IMUT;
   ];
   fun s -> try Hashtbl.find h s with Not_found -> IDENT s
}


let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (letter | '_') (letter | digit | '_')*
let integer = '0' | ['1'-'9'] digit*
let space = ' ' | '\t'
let comment = "#" [^'\n']*
let str_char = [^ '"' '\\' '\n']



rule token = parse
 | space+    {token lexbuf}
 | comment   {token lexbuf}
 | '\n'      {new_line lexbuf; token lexbuf}
 | integer   {CST (Cint (int_of_string (lexeme lexbuf)))}
 | '"' (str_char* as s) '"' {CST (Cstring s)}
 | ident     {id_or_kwg (lexeme lexbuf)}
 | "+"       {PLUS}
 | "->"      {ARROW}
 | "-"       {MINUS}
 | "*"       {TIMES}
 | "/"       {DIV}
 | "%"       {MOD}
 | "^"       {POW}
 | "=="      {EQ}
 | "!="      {NEQ}
 | "<="      {LEQ}
 | ">="      {GEQ}
 | "<"       {LT}
 | ">"       {GT}
 | "="       {ASSIGN}
 | "("       {LP}
 | ")"       {RP}
 | "{"       {LCURLY}
 | "}"       {RCURLY}
 | "["       {LBT}
 | "]"       {RBT}
 | ":"       {COLON}
 | ";"       {SEMI}
 | ","       {COMMA}
 | eof       {EOF}
 | _ as c    {raise (Lexing_error (Printf.sprintf "unexpected character: %c" c))}

  
