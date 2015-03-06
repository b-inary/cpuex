
(* flatten let-bindings (just for prettier printing) *)

open KNormal

(* ネストしたletの簡約 *)
let rec f = function
  | IfEq (x, y, e1, e2) -> IfEq (x, y, f e1, f e2)
  | IfNe (x, y, e1, e2) -> IfNe (x, y, f e1, f e2)
  | IfZ (x, e1, e2) -> IfZ (x, f e1, f e2)
  | IfNz (x, e1, e2) -> IfNz (x, f e1, f e2)
  | Let (xt, e1, e2) ->
      let rec insert = function
        | Let (yt, e3, e4) -> Let (yt, e3, insert e4)
        | LetRec (fundefs, e) -> LetRec (fundefs, insert e)
        | LetTuple (yts, z, e) -> LetTuple (yts, z, insert e)
        | e -> Let (xt, e, f e2) in
      insert (f e1)
  | LetRec ({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec ({ name = xt; args = yts; body = f e1 }, f e2)
  | LetTuple (xts, y, e) -> LetTuple (xts, y, f e)
  | e -> e

