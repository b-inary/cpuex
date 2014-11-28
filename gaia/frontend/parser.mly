%{
open Ast
%}

/* tokens */
%token <bool>   BOOL
%token <int>    INT
%token <float>  FLOAT
%token <string> IDENT
%token LPAR RPAR
%token PLUS MINUS AST SLASH SHL SHR
%token PLUSDOT MINUSDOT ASTDOT SLASHDOT FABS SQRT
%token NOT EQUAL NOTEQ LESS LESSEQ GREATER GREATEREQ
%token IF THEN ELSE LET IN REC COMMA SEMI SEMISEMI
%token DOT ASSIGN MAKEARRAY
%token READ WRITE ITOF FTOI FLOOR CASTINT CASTFLT
%token EOF

/* priority (from lower to higher) */
%nonassoc below_SEMI
%nonassoc SEMI
%nonassoc LET
%nonassoc ELSE
%nonassoc ASSIGN
%nonassoc below_COMMA
%left     COMMA
%left     EQUAL NOTEQ LESS LESSEQ GREATER GREATEREQ
%left     PLUS MINUS PLUSDOT MINUSDOT
%left     AST SLASH ASTDOT SLASHDOT
%right    SHL SHR
%nonassoc unary_minus

/* start symbol */
%type <Ast.ast> top
%start top

%%

top:
    seq_expr EOF  { $1 }
  | stmts         { $1 }
  | error         { failwith "parse" }

stmts:
    EOF                   { Atom Unit }
  | SEMISEMI EOF          { Atom Unit }
  | SEMISEMI seq_expr EOF { $2 }
  | SEMISEMI let_stmt     { $2 }
  | let_stmt              { $1 }

let_stmt:
    LET IDENT     EQUAL seq_expr stmts        { Let ($2, $4, $5) }
  | LET LPAR RPAR EQUAL seq_expr stmts        { Seq ($5, $6) }
  | LET     IDENT params EQUAL seq_expr stmts { LetFun ($2, $3, $5, $6) }
  | LET REC IDENT params EQUAL seq_expr stmts { LetFun ($3, $4, $6, $7) }
  | LET pat EQUAL seq_expr stmts              { LetTpl ($2, $4, $5) }

seq_expr:
    expr %prec below_SEMI { $1 }
  | expr SEMI             { $1 }
  | expr SEMI seq_expr    { Seq ($1, $3) }

expr:
    simple_expr         { $1 }
  | NOT simple_expr     { Not $2 }
  | MINUS expr %prec unary_minus
      { match $2 with
          | Atom (Int i) -> Atom (Int (-i))
          | Atom (Float f) -> Atom (Float (-.f))
          | _ -> IOp (Sub, Atom (Int 0), $2) }
  | expr PLUS expr      { IOp (Add, $1, $3) }
  | expr MINUS expr     { IOp (Sub, $1, $3) }
  | expr AST expr       { IOp (Mul, $1, $3) }
  | expr SLASH expr     { IOp (Div, $1, $3) }
  | expr SHL expr       { IOp (Shl, $1, $3) }
  | expr SHR expr       { IOp (Shr, $1, $3) }
  | MINUSDOT expr %prec unary_minus { FOp (FSub, Atom (Float 0.), $2) }
  | expr PLUSDOT expr   { FOp (FAdd, $1, $3) }
  | expr MINUSDOT expr  { FOp (FSub, $1, $3) }
  | expr ASTDOT expr    { FOp (FMul, $1, $3) }
  | expr SLASHDOT expr  { FOp (FDiv, $1, $3) }
  | FABS simple_expr    { FAbs ($2) }
  | SQRT simple_expr    { FSqrt ($2) }
  | expr EQUAL expr     { Cmp (EQ, $1, $3) }
  | expr NOTEQ expr     { Cmp (NE, $1, $3) }
  | expr LESS expr      { Cmp (LT, $1, $3) }
  | expr LESSEQ expr    { Cmp (LE, $1, $3) }
  | expr GREATER expr   { Cmp (GT, $1, $3) }
  | expr GREATEREQ expr { Cmp (GE, $1, $3) }
  | simple_expr args    { App ($1, $2) }
  | elems %prec below_COMMA { Tuple $1 }
  | MAKEARRAY simple_expr simple_expr               { MakeAry ($2, $3) }
  | simple_expr DOT LPAR seq_expr RPAR ASSIGN expr  { Put ($1, $4, $7) }
  | IF expr THEN expr ELSE expr                     { If ($2, $4, $6) }
  | LET IDENT     EQUAL seq_expr IN seq_expr        { Let ($2, $4, $6) }
  | LET LPAR RPAR EQUAL seq_expr IN seq_expr        { Seq ($5, $7) }
  | LET     IDENT params EQUAL seq_expr IN seq_expr { LetFun ($2, $3, $5, $7) }
  | LET REC IDENT params EQUAL seq_expr IN seq_expr { LetFun ($3, $4, $6, $8) }
  | LET pat EQUAL seq_expr IN seq_expr              { LetTpl ($2, $4, $6) }
  | READ simple_expr    { Dir (Read, $2) }
  | WRITE simple_expr   { Dir (Write, $2) }
  | ITOF simple_expr    { Dir (ItoF, $2) }
  | FTOI simple_expr    { Dir (FtoI, $2) }
  | FLOOR simple_expr   { Dir (Floor, $2) }
  | CASTINT simple_expr { Dir (CastInt, $2) }
  | CASTFLT simple_expr { Dir (CastFloat, $2) }

simple_expr:
    LPAR seq_expr RPAR { $2 }
  | LPAR RPAR { Atom Unit }
  | BOOL      { Atom (Bool $1) }
  | INT       { Atom (Int $1) }
  | FLOAT     { Atom (Float $1) }
  | IDENT     { Atom (Var $1) }
  | simple_expr DOT LPAR seq_expr RPAR { Get ($1, $4) }

args:
    simple_expr       { [$1] }
  | args simple_expr  { $1 @ [$2] }

elems:
    expr COMMA expr   { [$1; $3] }
  | elems COMMA expr  { $1 @ [$3] }

params:
    IDENT             { [$1] }
  | LPAR RPAR         { ["Unit"] }
  | IDENT params      { $1 :: $2 }
  | LPAR RPAR params  { "Unit" :: $3 }

pat:
    vars              { $1 }
  | LPAR vars RPAR    { $2 }

vars:
    IDENT COMMA IDENT { [$1; $3] }
  | vars COMMA IDENT  { $1 @ [$3] }

