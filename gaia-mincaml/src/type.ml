
(* MinCamlの型を表現するデータ型 *)
type t =
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t   (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | Var of t option ref

(* 新しい型変数を作る *)
let gentyp () = Var (ref None)

