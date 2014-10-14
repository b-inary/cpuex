
(* MinCamlの構文を表現するデータ型 *)
type t =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | FNeg of t
  | FAbs of t
  | FAdd of t * t
  | FMul of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t

and fundef =
  { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

let rec to_string = function
  | Unit -> "()"
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Not e -> "(Not " ^ (to_string e) ^ ")"
  | Neg e -> "(Neg " ^ (to_string e) ^ ")"
  | Add (e1, e2) -> "(Add " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")"
  | Sub (e1, e2) -> "(Sub " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")"
  | Mul (e1, e2) -> "(Mul " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")"
  | Div (e1, e2) -> "(Div " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")"
  | FNeg e -> "(FNeg " ^ (to_string e) ^ ")"
  | FAbs e -> "(FAbs " ^ (to_string e) ^ ")"
  | FAdd (e1, e2) -> "(FAdd " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")"
  | FMul (e1, e2) -> "(FMul " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")"
  | Eq (e1, e2) -> "(Eq " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")"
  | LE (e1, e2) -> "(LE " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")"
  | If (e1, e2, e3) -> "(If " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ", " ^ (to_string e3) ^ ")"
  | Let ((id, _), e1, e2) -> "(Let " ^ id ^ ", " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")"
  | Var id -> "(Var " ^ id ^ ")"
  | LetRec ({name=(n,_);body=e1}, e2) -> "(LetRec " ^ n ^ ", " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")"
  | App (e1, es) -> "(App " ^ (to_string e1) ^ ", " ^ (list_to_string es) ^ ")"
  | Tuple es -> "(Tuple " ^ (list_to_string es) ^ ")"
  | LetTuple (_, e1, e2) -> "(LetTuple _, " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")"
  | Array (e1, e2) -> "(Array " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")"
  | Get (e1, e2) -> "(Get " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ")"
  | Put (e1, e2, e3) -> "(Put " ^ (to_string e1) ^ ", " ^ (to_string e2) ^ ", " ^ (to_string e3) ^ ")"

and list_to_string = function
  | [] -> ""
  | [e] -> to_string e
  | e::es -> (to_string e) ^ ", " ^ (list_to_string es)
