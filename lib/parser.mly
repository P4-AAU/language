/* Parser for the language - minimal version */ 

%{
  open Ast 
%}

%token <string> IDENT
%token <Ast.constant> CST 
%token DEFINE OF TO FOR IN IF ELSE PRINT MATCH WITH RETURN
%token AND OR NOT
%token PLUS MINUS TIMES DIV MOD
%token EQ NEQ LT LEQ GT GEQ
%token ASSIGN
%token LP RP LCURLY RCURLY LBT RBT
%token COLON COMMA SEMI FACCES ARROW
/* %token BUFFER */
%token EOF
%token INT8 INT16 INT32 INT64
%token UINT8 UINT16 UINT32 UINT64
%token BOOL ARRAY STRING BUFFER
%token LIFO FIFO

/* Precedence - lavest øverst, højest nederst */
%left OR
%left AND
%nonassoc NOT
%nonassoc EQ NEQ LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc unary_minus

%start <Ast.file> file

%%

file:
  | b = block EOF {b}

type:
   /* todo */ 
  | INT8 { Tint8 }
  | INT16 { Tint16 }
  | INT32 { Tint32 }
  | INT64 { Tint64 }
  | UINT8 { Tuint8 }
  | UINT16 { Tuint16 }
  | UINT32 { Tuint32 }
  | UINT64 { Tuint64 }
  | base = type LBT RBT { Tarray base }
  | BOOL { Tbool }
  | STRING { Tstring }
  | FIFO LP elem_ty = type COMMA size = expr RP { Tbuffer (FIFO, elem_ty, size) }
  | LIFO LP elem_ty = type COMMA size = expr RP { Tbuffer (LIFO, elem_ty, size) }


expr: 
  | c = CST                           {Ecst c}
  | id = ident                        {Eident id}
  | MINUS e1 = expr %prec unary_minus {Eunop (Uneg, e1)}
  | NOT e1 = expr                     {Eunop (Unot, e1)}
  | e1 = expr o = binop e2 = expr     {Ebinop (o , e1, e2)}
  | LP e = expr RP                    {e}
  | /* "[" (expr ("," expr)∗)? "]" todo */
  | /* "lengthof" "(" expr ")" todo */ 

block:
  | LCURLY s = nonempty_list(stmt) RCURLY {Sblock s}

stmt:
  | DEFINE id = ident OF ty = type ASSIGN e = expr SEMI { Sdefine (id,ty,e) }
  | id = ident ASSIGN e = expr SEMI { Sassign (id ,e ) }
  | id = ident LBT e1 = expr RBT ASSIGN e2=expr SEMI {Sassign_index (id, e1 ,e2) }
  | IF e = expr b1 = block ELSE b2 = block {Sif (e, b1, b2)}
  | PRINT LP args = separated_list(COMMA, expr) RP {Sprint args}
  | FOR id = ident IN e = expr b = block { Sfor (id, e, b) }
  | FOR id = ident IN e1 = expr TO e2 = expr b = block { Sforrange (id,e1,e2,b) }
  | MATCH e = expr WITH cs = nonempty_list(match_case) SEMI {Smatch (e, cs)}
  | RETURN e = expr SEMI { Sreturn(e) }
  | b = block {Sblock b}
  | BUFFER name = ident COLON ty = type { Sbuffer (name, ty) }


match_case:
  | ps = patterns ARROW s = stmt {(ps,s)}
;

param:
 /* todo */
  | id = ident OF ty = type { (id, ty) }

func_decl: 
 /* todo */
  | DEFINE ident OF type LP param COMMA param RP block

patterns:
/* todo */ 
  | c = CST { Pconst c }
  | id = ident {
    if id.id = "_" then Pwildcard
    else Pident id
  }
  | INT | BOOL | IDENT | STRING | '_'



%inline binop
  | PLUS  { Badd } 
  | MINUS { Bsub }
  | TIMES { Bmul }
  | DIV   { Bdiv } 
  | MOD   { Bmod }
  | EQ    { Beq  } 
  | NEQ   { Bneq }
  | LT    { Blt  } 
  | LE    { Ble  }
  | GT    { Bgt  } 
  | GE    { Bge  }
  | AND   { Band } 
  | OR    { Bor  }

ident:
  | id = IDENT { { loc = ($startpos, $endpos); id } }
