%{
open Util
open Llvm

type ty =
  | Scaler
  | Ptr of ty
  | Array of ty * int
  | Tuple of ty list

let rec size = function
  | Scaler | Ptr _ -> 1
  | Array (_, sz) -> sz
  | Tuple l -> List.sum (List.map size l)

let rec gep t = function
  | [Int n] ->
    begin match t with
    | Ptr t | Array (t, _) -> Int (n * size t)
    | Tuple ts -> Int (List.sum (List.map size (List.take n ts)))
    | Scaler -> failwith "parse"
    end
  | [Var v] ->
    begin match t with
    | Ptr t | Array (t, _) when size t = 1 -> Var v
    | Tuple ts when List.map size ts = List.map (const 1) ts -> Var v
    | _ -> failwith "parse"
    end
  | Int n :: xs ->
    begin match t with
    | Ptr t | Array (t, _) ->
      begin match gep t xs with
      | Int m -> Int (n * size t + m)
      | Var _ as v when n = 0 -> v
      | _ -> failwith "parse"
      end
    | Tuple ts ->
      begin match gep (List.nth ts n) xs with
      | Int m -> Int (List.sum (List.map size (List.take n ts)) + m)
      | Var _ as v when n = 0 -> v
      | _ -> failwith "parse"
      end
    | _ -> failwith "parse"
    end
  | _ -> failwith "parse"
%}

/* tokens */
%token <int> INT
%token <float> FLOAT
%token <string> VAR GLOBAL DEFLABEL
%token EOL EOF
%token EQUAL LPAREN RPAREN LBRACKET RBRACKET COMMA DEFINE LABEL
%token I1 I8 I32 I64 FLT VOID LBRACE RBRACE AST X
%token BR RET SWITCH
%token ADD SUB SHL SHR AND OR XOR FADD FSUB FMUL FDIV
%token LOAD STORE GEP BITCAST TO SITOFP ICMP FCMP SELECT CALL PHI
%token EQ NE LT LE GT GE

/* start symbol */
%type <Llvm.prog> top
%start top

%%

top:
  | eols globals funcs EOF  { ($2, $3) }
  | error                   { failwith "parse" }

globals:
  |                 { [] }
  | global globals  { $1 :: $2 }

global:
  | GLOBAL EQUAL typ eols  { ($1, size $3) }

funcs:
  | func        { [$1] }
  | func funcs  { $1 :: $2 }

func:
  | DEFINE typ GLOBAL LPAREN params RPAREN LBRACE eols blocks RBRACE eols
    { ($3, $5, $9) }
  | DEFINE VOID GLOBAL LPAREN params RPAREN LBRACE eols blocks RBRACE eols
    { ($3, $5, $9) }

params:
  |                       { [] }
  | typ VAR               { if size $1 = 1 then [$2] else failwith "parse" }
  | typ VAR COMMA params  { if size $1 = 1 then $2 :: $4 else failwith "parse" }

blocks:
  | block         { [$1] }
  | block blocks  { $1 :: $2 }

block:
  | insts terminator eols               { ("0", $1, $2) }
  | DEFLABEL eols insts terminator eols { ($1, $3, $4) }

insts:
  |             { [] }
  | inst insts  { $1 :: $2 }

inst:
  | VAR EQUAL def eols
    { Def ($1, $3) }
  | STORE typ value COMMA typ value eols
    { if size $2 = 1 then Store ($3, $6, Int 0) else failwith "parse" }
  | STORE typ value COMMA typ GEP LPAREN typ value idx RPAREN eols
    { if size $2 = 1 then Store ($3, $9, gep $8 $10) else failwith "parse" }
  | STORE I64 INT COMMA typ value eols
    { if $3 = 0 then Store2 $6 else failwith "parse" }
  | STORE I64 INT COMMA typ BITCAST LPAREN typ value TO typ RPAREN eols
    { if $3 = 0 then Store2 $9 else failwith "parse" }
  | CALL VOID GLOBAL LPAREN args RPAREN eols
    { CallVoid ($3, $5) }

def:
  | ADD I32 value COMMA value
    { Bin (Add, $3, $5) }
  | SUB I32 value COMMA value
    { Bin (Sub, $3, $5) }
  | SHL I32 value COMMA value
    { Bin (Shl, $3, $5) }
  | SHR I32 value COMMA value
    { Bin (Shr, $3, $5) }
  | AND I1 value COMMA value
    { Bin (And, $3, $5) }
  | AND I32 value COMMA value
    { Bin (And, $3, $5) }
  | OR I1 value COMMA value
    { Bin (Or, $3, $5) }
  | OR I32 value COMMA value
    { Bin (Or, $3, $5) }
  | XOR I1 value COMMA value
    { Bin (Xor, $3, $5) }
  | XOR I32 value COMMA value
    { Bin (Xor, $3, $5) }
  | FADD FLT value COMMA value
    { Bin (FAdd, $3, $5) }
  | FSUB FLT value COMMA value
    { Bin (FSub, $3, $5) }
  | FMUL FLT value COMMA value
    { Bin (FMul, $3, $5) }
  | FDIV FLT value COMMA value
    { Bin (FDiv, $3, $5) }
  | LOAD typ value
    { if size $2 = 1 then Load ($3, Int 0) else failwith "parse" }
  | LOAD typ GEP LPAREN typ value idx RPAREN
    { if size $2 = 1 then Load ($6, gep $5 $7) else failwith "parse" }
  | GEP typ value idx
    { Gep ($3, gep $2 $4) }
  | BITCAST typ VAR TO typ
    { Mov $3 }
  | SITOFP I32 value TO FLT
    { IToF $3 }
  | ICMP EQ typ value COMMA value
    { Bin (EQ, $4, $6) }
  | ICMP NE typ value COMMA value
    { Bin (NE, $4, $6) }
  | ICMP LT typ value COMMA value
    { Bin (LT, $4, $6) }
  | ICMP LE typ value COMMA value
    { Bin (LE, $4, $6) }
  | ICMP GT typ value COMMA value
    { Bin (GT, $4, $6) }
  | ICMP GE typ value COMMA value
    { Bin (GE, $4, $6) }
  | FCMP EQ typ value COMMA value
    { Bin (FEQ, $4, $6) }
  | FCMP NE typ value COMMA value
    { Bin (FNE, $4, $6) }
  | FCMP LT typ value COMMA value
    { Bin (FLT, $4, $6) }
  | FCMP LE typ value COMMA value
    { Bin (FLE, $4, $6) }
  | FCMP GT typ value COMMA value
    { Bin (FGT, $4, $6) }
  | FCMP GE typ value COMMA value
    { Bin (FGE, $4, $6) }
  | SELECT I1 VAR COMMA typ value COMMA typ value
    { Select ($3, $6, $9) }
  | CALL typ GLOBAL LPAREN args RPAREN
    { Call ($3, $5) }
  | PHI typ phi
    { Phi $3 }

idx:
  | COMMA typ value      { [$3] }
  | COMMA typ value idx  { $3 :: $4 }

args:
  |                 { [] }
  | arg             { [$1] }
  | arg COMMA args  { $1 :: $3 }

arg:
  | typ value
    { if size $1 = 1 then $2 else failwith "parse" }
  | typ GEP LPAREN typ value idx RPAREN
    { if size $1 = 1 && gep $4 $6 = Int 0 then $5 else failwith "parse" }
  | typ BITCAST LPAREN typ value TO typ RPAREN
    { if size $7 = 1 then $5 else failwith "parse" }

phi:
  | LBRACKET value COMMA VAR RBRACKET           { [($2, $4)] }
  | LBRACKET value COMMA VAR RBRACKET COMMA phi { ($2, $4) :: $7 }

terminator:
  | BR LABEL VAR
    { Jump $3 }
  | BR I1 VAR COMMA LABEL VAR COMMA LABEL VAR
    { Branch ($3, $6, $9) }
  | RET typ value
    { Ret $3 }
  | RET VOID
    { RetVoid }
  | SWITCH typ VAR COMMA LABEL VAR LBRACKET eols switch RBRACKET
    { Switch ($3, $6, $9) }

switch:
  | I32 INT COMMA LABEL VAR eols
    { [($2, $5)] }
  | I32 INT COMMA LABEL VAR eols switch
    { ($2, $5) :: $7 }

typ:
  | I1      { Scaler }
  | I8      { Scaler }
  | I32     { Scaler }
  | FLT     { Scaler }
  | typ AST { Ptr $1 }
  | I64 AST { Ptr Scaler }
  | LBRACKET INT X typ RBRACKET { Array ($4, $2) }
  | LBRACE typs RBRACE          { Tuple $2 }

typs:
  | typ             { [$1] }
  | typ COMMA typs  { $1 :: $3 }

value:
  | INT     { Int $1 }
  | FLOAT   { Float $1 }
  | VAR     { Var $1 }
  | GLOBAL  { Global $1 }

eols:
  | EOL       {}
  | EOL eols  {}

