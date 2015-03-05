
type alu3op =
  | FCmpNE | FCmpEQ | FCmpLT | FCmpLE | FCmpGT | FCmpGE

type alu4op =
  | Add | Sub | Shl | Shr | Sar | And | Or | Xor
  | CmpNE | CmpEQ | CmpLT | CmpLE | CmpGT | CmpGE

type fpu2op =
  | FSqrt | FToI | IToF | Floor

type fpu3op =
  | FAdd | FSub | FMul | FDiv

type sign =
  | Nop | Abs | Neg | Minus

type def =
  | Mov of string
  | MovI of int
  | MovF of float
  | Alu3 of alu3op * string * string
  | Alu4 of alu4op * string * string * int
  | Fpu2 of fpu2op * sign * string
  | Fpu3 of fpu3op * sign * string * string
  | Load of string * int
  | Read
  | Call of string * string list
  | Phi of (string * string) list

type inst =
  | Def of string * def
  | Store of string * string * int
  | Write of string
  | CallVoid of string * string list
  | Jump of string
  | Branch of string * string * string
  | Switch of string * string * (int * string) list
  | Ret of string
  | RetVoid
  | TailCall of string * string list

type block = string * inst list
type func = string * string list * block list
