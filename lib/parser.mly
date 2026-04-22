/* Parser for the language - minimal version */ 

%{
  open Ast
  let mk_expr s e node = { expr_loc = (s, e); expr_node = node }
%}

%token <string> IDENT
%token <Ast.constant> CST
%token DEFINE OF TO FOR IN IF ELSE PRINT MATCH WITH RETURN LENGTHOF DELETE INPUT
%token AND OR NOT
%token PLUS MINUS TIMES DIV MOD POW SQRT
%token EQ NEQ LT LEQ GT GEQ
%token ASSIGN
%token LP RP LCURLY RCURLY LBT RBT
%token COLON COMMA SEMI FACCES ARROW
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
%right POW SQRT
%nonassoc unary_minus

%start <Ast.file> file

%%

file:
  | b = block EOF { b }

typ:
  | INT8 { Tint8 }
  | INT16 { Tint16 }
  | INT32 { Tint32 }
  | INT64 { Tint64 }
  | UINT8 { Tuint8 }
  | UINT16 { Tuint16 }
  | UINT32 { Tuint32 }
  | UINT64 { Tuint64 }
  | base = typ LBT RBT { Tarray base }
  | BOOL { Tbool }
  | STRING { Tstring }
  | FIFO LP elem_ty = typ COMMA size = expr RP { Tbuffer (FIFO, elem_ty, size) }
  | LIFO LP elem_ty = typ COMMA size = expr RP { Tbuffer (LIFO, elem_ty, size) }

expr:
  | c = CST                                          { mk_expr $startpos $endpos (Ecst c) }
  | id = ident                                       { mk_expr $startpos $endpos (Eident id) }
  | MINUS e1 = expr %prec unary_minus                { mk_expr $startpos $endpos (Eunop (Uneg, e1)) }
  | NOT e1 = expr                                    { mk_expr $startpos $endpos (Eunop (Unot, e1)) }
  | SQRT e1 = expr                                   { mk_expr $startpos $endpos (Eunop (Usqrt, e1)) }
  | e1 = expr o = binop e2 = expr                    { mk_expr $startpos $endpos (Ebinop (o, e1, e2)) }
  | e = expr LBT idx = expr RBT                      { mk_expr $startpos $endpos (Eindex (e, idx)) }
  | e = expr LBT s = expr COLON t = expr RBT         { mk_expr $startpos $endpos (Eslice (e, s, t)) }
  | LBT es = separated_list(COMMA, expr) RBT         { mk_expr $startpos $endpos (Earray es) }
  | LENGTHOF LP e = expr RP                          { mk_expr $startpos $endpos (Elength e) }
  | LP e = expr RP                                   { e }

block:
  | LCURLY s = nonempty_list(stmt) RCURLY { s }

stmt:
  | DEFINE id = ident OF ty = typ ASSIGN e = expr SEMI { Sdefine (id, ty, e) }
  | id = ident ASSIGN e = expr SEMI { Sassign (id, e) }
  | id = ident LBT e1 = expr RBT ASSIGN e2 = expr SEMI { Sassign_index (id, e1, e2) }
  | IF e = expr b1 = block ELSE b2 = block { Sif (e, Sblock b1, Sblock b2) }
  | PRINT LP args = separated_list(COMMA, expr) RP SEMI { Sprint args }
  | FOR id = ident IN e = expr b = block { Sfor (id, e, Sblock b) }
  | FOR id = ident IN e1 = expr TO e2 = expr b = block { Sforrange (id, e1, e2, Sblock b) }
  | MATCH e = expr WITH cs = nonempty_list(match_case) SEMI { Smatch (e, cs) }
  | RETURN e = expr SEMI { Sreturn e }
  | INPUT id = ident COLON ty = typ SEMI { Sinput (id, ty) }
  | DELETE id = ident SEMI { Sdelete id }
  | f = func_decl { f }
  | b = block { Sblock b }
  | BUFFER name = ident COLON ty = typ ASSIGN size = expr SEMI { Sbuffer (name, ty, size, []) }

match_case:
  | ps = patterns ARROW s = stmt { (ps, s) }

param:
  | id = ident OF ty = typ { (id, ty) }

func_decl:
  | DEFINE id = ident OF ret = typ LP params = separated_list(COMMA, param) RP body = block
    { Sfunc (id, ret, params, Sblock body) }

patterns:
  | c = CST { Pconst c }
  | id = ident {
      if id.id = "_" then Pwildcard
      else Pident id
    }

%inline binop:
  | PLUS  { Badd } 
  | MINUS { Bsub }
  | TIMES { Bmul }
  | DIV { Bdiv }
  | MOD { Bmod }
  | POW { Bpow }
  | EQ { Beq }
  | NEQ { Bneq }
  | LT { Blt }
  | LEQ { Ble }
  | GT { Bgt }
  | GEQ { Bge }
  | AND { Band }
  | OR { Bor }

ident:
  | id = IDENT { { loc = ($startpos, $endpos); id } }
