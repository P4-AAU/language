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
     "while",  WHILE;
     "print",  PRINT;
     "and",    AND;
     "or",     OR;
     "not",    NOT;
     "true",   CST (Cbool true);
     "false",  CST (Cbool false);
     "return", RET;
     "function", FUNC;
     "lengthof", LENGTHOF;
     "match", MATCH; 
     "for", FOR;
   ];
   fun s -> try Hashtbl.find h s with Not_found -> IDENT s
}


let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (letter | '_') (letter | digit | '_')*
let integer = '0' | ['1'-'9'] digit*
let space = ' ' | '\t'
let comment = "#" [^'\n']*



rule token = parse
 | space+    {token lexbuf}
 | comment   {token lexbuf}
 | '\n'      {new_line lexbuf; token lexbuf}
 | integer   {CST (Cint (int_of_string (lexeme lexbuf)))}
 | ident     {id_or_kwg (lexeme lexbuf)}
 | "+"       {PLUS}
 | "-"       {MINUS}
 | "*"       {TIMES}
 | "/"       {DIV}
 | "%"       {MOD}
 | "^"       {POW} 
 | "=="      {EQ}
 | "!="      {NEQ}
 | "<"       {LT}
 | "<="      {LE}
 | ">"       {GT}
 | ">="      {GE}
 | "="       {ASSIGN}
 | "("       {LP}
 | ")"       {RP}
 | "{"       {LBE}
 | "}"       {RBE}
 | "["       {LBT}
 | "]"       {RBT}
 | ":"       {COLON}
 | ";"       {SEMI}
 | ","       {COMMA}
 | eof       {EOF}
 | _ as c    {raise (Lexing_error (Printf.sprintf "unexpected character: %c" c))}

  
