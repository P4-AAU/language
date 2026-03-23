/* Parser for the language - minimal version */ 

%{
  open Ast 
%}

%token <string> IDENT
%token <Ast.constant> CST 
%token IF ELSE WHILE PRINT
%token AND OR NOT
%token PLUS MINUS TIMES DIV MOD
%token EQ NEQ LT LE GT GE 
%token ASSIGN
%token LP RP
%token COLON COMMA
%token EOF

/* Precedence - lavest øverst, højest nederst */
%left OR
%left AND
%nonassoc NOT
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc unary_minus

%start <Ast.file> file

%%

file:
  | s = stmt EOF {s}

stmt:
  | WHILE e = expr COLON s = stmt {Swhile (e, s)}
  | IF e = expr COLON s1 = stmt ELSE COLON s2 = stmt {Sif (e, s1, s2)}
  | PRINT LP args = separated_list(COMMA, expr) RP {Sprint args}
  | id = ident ASSIGN e = expr {Sassign (id, e)}
  | s = nonempty_list(stmt) {Sblock s}

expr: 
  | c = CST                          {Ecst c}
  | id = ident                       {Eident id}
  | MINUS e = expr %prec unary_minus {Eunop (Uneg, e)}
  | NOT e = expr                     {Eunop (Unot, e)}
  | e1 = expr o = binop e2 = expr    {Ebinop (o , e1, e2)}
  | LP e = expr RP                   {e}

%inline binop:
  | PLUS  { Badd } | MINUS { Bsub } | TIMES { Bmul }
  | DIV   { Bdiv } | MOD   { Bmod }
  | EQ    { Beq  } | NEQ   { Bneq }
  | LT    { Blt  } | LE    { Ble  }
  | GT    { Bgt  } | GE    { Bge  }
  | AND   { Band } | OR    { Bor  }

ident:
  | id = IDENT { { loc = ($startpos, $endpos); id } }
