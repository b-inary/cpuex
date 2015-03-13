%{
  open Syntax
  let addtyp x = (x, Type.gentyp ())
%}

%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token NOT
%token PLUS MINUS
%token AST SLASH
%token PLUS_DOT MINUS_DOT
%token AST_DOT SLASH_DOT
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token <Id.t> IDENT
%token LET
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token XOR
%token FEQUAL FLESS FISPOS FISNEG FISZERO
%token FHALF FSQR FABS FNEG FTOI ITOF FLOOR
%token EOF

/* 優先順位とassociativityの定義 (低い方から高い方へ) */
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%left COMMA
%left EQUAL LESS_GREATER LESS_EQUAL GREATER_EQUAL LESS GREATER
%left PLUS MINUS PLUS_DOT MINUS_DOT
%left AST SLASH AST_DOT SLASH_DOT
%right prec_unary_minus
%left prec_app
%left DOT

/* 開始記号の定義 */
%type <Syntax.t> exp
%start exp

%%

/* 括弧をつけなくても関数の引数になれる式 */
simple_exp:
| LPAREN exp RPAREN                 { $2 }
| LPAREN RPAREN                     { Unit }
| BOOL                              { Bool  $1 }
| INT                               { Int   $1 }
| FLOAT                             { Float $1 }
| IDENT                             { Var   $1 }
| simple_exp DOT LPAREN exp RPAREN  { Get ($1, $4) }

/* 一般の式 */
exp:
| simple_exp                { $1 }
| NOT exp
    %prec prec_app          { Not $2 }
| MINUS exp
    %prec prec_unary_minus  { match $2 with
                                | Int   i -> Int (-i)
                                | Float f -> Float (-.f)
                                | e -> Neg e }
| exp PLUS exp              { Add ($1, $3) }
| exp MINUS exp             { Sub ($1, $3) }
| exp AST exp               { Mul ($1, $3) }
| exp SLASH exp             { Div ($1, $3) }
| exp EQUAL exp             { Eq ($1, $3) }
| exp LESS_GREATER exp      { Ne ($1, $3) }
| exp LESS exp              { Lt ($1, $3) }
| exp GREATER exp           { Lt ($3, $1) }
| exp LESS_EQUAL exp        { Le ($1, $3) }
| exp GREATER_EQUAL exp     { Le ($3, $1) }
| IF exp THEN exp ELSE exp
    %prec prec_if           { If ($2, $4, $6) }
| MINUS_DOT exp
    %prec prec_unary_minus  { FNeg $2 }
| exp PLUS_DOT exp          { FAdd ($1, $3) }
| exp MINUS_DOT exp         { FSub ($1, $3) }
| exp AST_DOT exp           { FMul ($1, $3) }
| exp SLASH_DOT exp         { FMul ($1, App (Var "finv", [$3])) }
| LET IDENT EQUAL exp IN exp
    %prec prec_let          { Let (addtyp $2, $4, $6) }
| LET REC fundef IN exp
    %prec prec_let          { LetRec ($3, $5) }
| exp actual_args
    %prec prec_app          { App ($1, $2) }
| elems                     { Tuple $1 }
| LET LPAREN pat RPAREN EQUAL exp IN exp
                            { LetTuple ($3, $6, $8) }
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp
                            { Put ($1, $4, $7) }
| exp SEMICOLON exp         { Let ((Id.gentmp Type.Unit, Type.Unit), $1, $3) }
| exp SEMICOLON             { Let ((Id.gentmp Type.Unit, Type.Unit), $1, Unit) }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app          { Array ($2, $3) }
| XOR simple_exp simple_exp
    %prec prec_app          { Ne ($2, $3) }
| FEQUAL simple_exp simple_exp
    %prec prec_app          { Eq ($2, $3) }
| FLESS simple_exp simple_exp
    %prec prec_app          { FLt ($2, $3) }
| FISPOS simple_exp
    %prec prec_app          { FLt (Float 0.0, $2) }
| FISNEG simple_exp
    %prec prec_app          { FLt ($2, Float 0.0) }
| FISZERO simple_exp
    %prec prec_app          { Eq ($2, Float 0.0) }
| FHALF simple_exp
    %prec prec_app          { FMul ($2, Float 0.5) }
| FSQR simple_exp
    %prec prec_app          { FMul ($2, $2) }
| FABS simple_exp
    %prec prec_app          { FAbs $2 }
| FNEG simple_exp
    %prec prec_app          { FNeg $2 }
| FTOI simple_exp
    %prec prec_app          { FToI $2 }
| ITOF simple_exp
    %prec prec_app          { IToF $2 }
| FLOOR simple_exp
    %prec prec_app          { Floor $2 }
| error
    { failwith
        (Printf.sprintf "\nerror: parse error near characters %d-%d\n"
          (Parsing.symbol_start ())
          (Parsing.symbol_end ())) }

fundef:
| IDENT formal_args EQUAL exp
  { { name = addtyp $1; args = $2; body = $4 } }

formal_args:
| IDENT formal_args { addtyp $1 :: $2 }
| IDENT             { [addtyp $1] }

actual_args:
| actual_args simple_exp %prec prec_app { $1 @ [$2] }
| simple_exp             %prec prec_app { [$1] }

elems:
| elems COMMA exp { $1 @ [$3] }
| exp COMMA exp   { [$1; $3] }

pat:
| pat COMMA IDENT   { $1 @ [addtyp $3] }
| IDENT COMMA IDENT { [addtyp $1; addtyp $3] }

