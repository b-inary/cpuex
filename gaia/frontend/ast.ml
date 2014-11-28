
open Printf

type iop = Add | Sub | Mul | Div | Shl | Shr
type fop = FAdd | FSub | FMul | FDiv
type relop  = EQ | NE | LT | LE | GT | GE
type dir = Read | Write | ItoF | FtoI | Floor | CastInt | CastFloat

type atom = Unit | Bool of bool | Int of int | Float of float | Var of string

(* Abstract Syntax Tree *)
type ast =
    Atom    of atom
  | Not     of ast
  | IOp     of iop * ast * ast
  | FOp     of fop * ast * ast
  | FAbs    of ast
  | FSqrt   of ast
  | Cmp     of relop * ast * ast
  | App     of ast * ast list
  | Tuple   of ast list
  | MakeAry of ast * ast
  | Get     of ast * ast
  | Put     of ast * ast * ast
  | If      of ast * ast * ast
  | Let     of string * ast * ast
  | LetFun  of string * string list * ast * ast
  | LetTpl  of string list * ast * ast
  | Seq     of ast * ast
  | Dir     of dir * ast

let atom_to_string = function
    Unit -> "()"
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Var v -> v

let ast_to_string a =
  let join = String.concat in
  let rec go fmt = function
      Atom a ->                 atom_to_string a
    | Not e ->                  sprintf "(not %a)" go e
    | IOp (Add, e1, e2) ->      sprintf "(%a + %a)" go e1 go e2
    | IOp (Sub, e1, e2) ->      sprintf "(%a - %a)" go e1 go e2
    | IOp (Mul, e1, e2) ->      sprintf "(%a * %a)" go e1 go e2
    | IOp (Div, e1, e2) ->      sprintf "(%a / %a)" go e1 go e2
    | IOp (Shl, e1, e2) ->      sprintf "(%a << %a)" go e1 go e2
    | IOp (Shr, e1, e2) ->      sprintf "(%a >> %a)" go e1 go e2
    | FOp (FAdd, e1, e2) ->     sprintf "(%a +. %a)" go e1 go e2
    | FOp (FSub, e1, e2) ->     sprintf "(%a -. %a)" go e1 go e2
    | FOp (FMul, e1, e2) ->     sprintf "(%a *. %a)" go e1 go e2
    | FOp (FDiv, e1, e2) ->     sprintf "(%a /. %a)" go e1 go e2
    | FAbs e ->                 sprintf "(fabs %a)" go e
    | FSqrt e ->                sprintf "(sqrt %a)" go e
    | Cmp (EQ, e1, e2) ->       sprintf "(%a = %a)" go e1 go e2
    | Cmp (NE, e1, e2) ->       sprintf "(%a <> %a)" go e1 go e2
    | Cmp (LT, e1, e2) ->       sprintf "(%a < %a)" go e1 go e2
    | Cmp (LE, e1, e2) ->       sprintf "(%a <= %a)" go e1 go e2
    | Cmp (GT, e1, e2) ->       sprintf "(%a > %a)" go e1 go e2
    | Cmp (GE, e1, e2) ->       sprintf "(%a >= %a)" go e1 go e2
    | App (e, l) ->             sprintf "(%a %a)" go e go_list (l, " ")
    | Tuple l ->                sprintf "(%a)" go_list (l, ", ")
    | MakeAry (e1, e2) ->       sprintf "(create_array %a %a)" go e1 go e2
    | Get (e1, e2) ->           sprintf "(%a.(%a))" go e1 go e2
    | Put (e1, e2, e3) ->       sprintf "(%a.(%a) <- %a)" go e1 go e2 go e3
    | If (e1, e2, e3) ->        sprintf "(if %a then %a else %a)" go e1 go e2 go e3
    | Let (v, e1, e2) ->        sprintf "let %s = %a in %a" v go e1 go e2
    | LetFun (v, l, e1, e2) ->  sprintf "let %s %s = %a in %a" v (join " " l) go e1 go e2
    | LetTpl (l, e1, e2) ->     sprintf "let (%s) = %a in %a" (join ", " l) go e1 go e2
    | Seq (e1, e2) ->           sprintf "%a; %a" go e1 go e2
    | Dir (Read, e) ->          sprintf "(#read %a)" go e
    | Dir (Write, e) ->         sprintf "(#write %a)" go e
    | Dir (ItoF, e) ->          sprintf "(#itof %a)" go e
    | Dir (FtoI, e) ->          sprintf "(#ftoi %a)" go e
    | Dir (Floor, e) ->         sprintf "(#floor %a)" go e
    | Dir (CastInt, e) ->       sprintf "(#castint %a)" go e
    | Dir (CastFloat, e) ->     sprintf "(#castfloat %a)" go e
  and go_list fmt = function
      [], _ -> ""
    | [e], _ ->                 sprintf "%a" go e
    | e::es, delim ->           sprintf "%a%s%a" go e delim go_list (es, delim) in
  go () a

