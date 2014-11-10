
type iunop  = Neg
type ibinop = Add | Sub | Mul | Div
type funop  = FNeg | FAbs
type fbinop = FAdd | FSub | FMul | FDiv
type relop  = EQ | NE | LT | LE | GT | GE

(* Abstract Syntax Tree *)
type ast =
    Unit
  | Bool    of bool
  | Int     of int
  | Float   of float
  | Var     of string
  | Not     of ast
  | IUnOp   of iunop * ast
  | IBinOp  of ibinop * ast * ast
  | FUnOp   of funop * ast
  | FBinOp  of fbinop * ast * ast
  | Cmp     of relop * ast * ast
  | Tuple   of ast list
  | MakeAry of ast * ast
  | Get     of ast * ast
  | Put     of ast * ast * ast
  | If      of ast * ast * ast
  | Let     of string * ast * ast
  | LetFun  of string * string list * ast * ast
  | LetTpl  of string list * ast * ast
  | App     of ast * ast list
  | Seq     of ast * ast
  | Read
  | Write   of ast
  | ItoF    of ast
  | FtoI    of ast
  | Floor   of ast
  | CastInt of ast
  | CastFlt of ast

let string_of_ast (a : ast) : string =
  let rec join fmt = function
      ([], _) ->                ""
    | ([s], _) ->               s
    | (s::ss, delim) ->         Format.sprintf "%s%s%a" s delim join (ss, delim) in
  let rec go fmt = function
      Unit ->                   "()"
    | Bool b ->                 string_of_bool b
    | Int i ->                  string_of_int i
    | Float f ->                string_of_float f
    | Var v ->                  v
    | Not e ->                  Format.sprintf "(Not %a)" go e
    | IUnOp (Neg, e) ->         Format.sprintf "(-%a)" go e
    | IBinOp (Add, e1, e2) ->   Format.sprintf "(%a + %a)" go e1 go e2
    | IBinOp (Sub, e1, e2) ->   Format.sprintf "(%a - %a)" go e1 go e2
    | IBinOp (Mul, e1, e2) ->   Format.sprintf "(%a * %a)" go e1 go e2
    | IBinOp (Div, e1, e2) ->   Format.sprintf "(%a / %a)" go e1 go e2
    | FUnOp (FNeg, e) ->        Format.sprintf "(-.%a)" go e
    | FUnOp (FAbs, e) ->        Format.sprintf "(FAbs %a)" go e
    | FBinOp (FAdd, e1, e2) ->  Format.sprintf "(%a +. %a)" go e1 go e2
    | FBinOp (FSub, e1, e2) ->  Format.sprintf "(%a -. %a)" go e1 go e2
    | FBinOp (FMul, e1, e2) ->  Format.sprintf "(%a *. %a)" go e1 go e2
    | FBinOp (FDiv, e1, e2) ->  Format.sprintf "(%a /. %a)" go e1 go e2
    | Cmp (EQ, e1, e2) ->       Format.sprintf "(%a = %a)" go e1 go e2
    | Cmp (NE, e1, e2) ->       Format.sprintf "(%a <> %a)" go e1 go e2
    | Cmp (LT, e1, e2) ->       Format.sprintf "(%a < %a)" go e1 go e2
    | Cmp (LE, e1, e2) ->       Format.sprintf "(%a <= %a)" go e1 go e2
    | Cmp (GT, e1, e2) ->       Format.sprintf "(%a > %a)" go e1 go e2
    | Cmp (GE, e1, e2) ->       Format.sprintf "(%a >= %a)" go e1 go e2
    | Tuple l ->                Format.sprintf "(%a)" go_list (l, ", ")
    | MakeAry (e1, e2) ->       Format.sprintf "(MakeArray %a %a)" go e1 go e2
    | Get (e1, e2) ->           Format.sprintf "(%a.(%a))" go e1 go e2
    | Put (e1, e2, e3) ->       Format.sprintf "(%a.(%a) <- %a)" go e1 go e2 go e3
    | If (e1, e2, e3) ->        Format.sprintf "(If %a Then %a Else %a)" go e1 go e2 go e3
    | Let (v, e1, e2) ->        Format.sprintf "(Let %s = %a In %a)" v go e1 go e2
    | LetFun (v, l, e1, e2) ->  Format.sprintf "(LetFun %s %a = %a In\n%a)" v join (l, " ") go e1 go e2
    | LetTpl (l, e1, e2) ->     Format.sprintf "(LetTuple (%a) = %a In %a)" join (l, ", ") go e1 go e2
    | App (e, l) ->             Format.sprintf "(Apply %a %a)" go e go_list (l, " ")
    | Seq (e1, e2) ->           Format.sprintf "(%a; %a)" go e1 go e2
    | Read ->                   "#Read"
    | Write e ->                Format.sprintf "(#Write %a)" go e
    | ItoF e ->                 Format.sprintf "(#ItoF %a)" go e
    | FtoI e ->                 Format.sprintf "(#FtoI %a)" go e
    | Floor e ->                Format.sprintf "(#Floor %a)" go e
    | CastInt e ->              Format.sprintf "(#CastInt %a)" go e
    | CastFlt e ->              Format.sprintf "(#CastFloat %a)" go e
  and go_list fmt = function
      ([], _) ->                ""
    | ([e], _) ->               Format.sprintf "%a" go e
    | (e::es, delim) ->         Format.sprintf "%a%s%a" go e delim go_list (es, delim) in
  go () a

