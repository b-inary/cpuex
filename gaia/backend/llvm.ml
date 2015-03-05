
type value =
  | Int of int
  | Float of float
  | Var of string
  | Global of string

type binop =
  | Add | Sub | Shl | Shr | And | Or | Xor
  | FAdd | FSub | FMul | FDiv
  | EQ | NE | LT | LE | GT | GE
  | FEQ | FNE | FLT | FLE | FGT | FGE

type def =
  | Mov of string
  | Bin of binop * value * value
  | IToF of value
  | Load of value * value
  | Gep of value * value
  | Select of string * value * value
  | Call of string * value list
  | Phi of (value * string) list

type inst =
  | Def of string * def
  | Store of value * value * value
  | Store2 of value
  | CallVoid of string * value list

type term =
  | Jump of string
  | Branch of string * string * string
  | Switch of string * string * (int * string) list
  | Ret of value
  | RetVoid

(* label name * body * terminator *)
type block = string * inst list * term

(* name * params * function body *)
type func = string * string list * block list

(* name * size *)
type global = string * int

(* program definition *)
type prog = global list * func list
