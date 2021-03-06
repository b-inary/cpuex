
open KNormal

(* インライン展開する関数の最大サイズ *)
(* Mainで-inlineオプションによりセットされる *)
let threshold = ref 0

let rec size = function
  | IfEq (_, _, e1, e2) | IfLE (_, _, e1, e2) | IfEqZ (_, e1, e2) | IfLEZ (_, e1, e2) | IfGEZ (_, e1, e2)
  | Let (_, e1, e2) | LetRec ({ body = e1 }, e2) ->
      1 + size e1 + size e2
  | LetTuple (_, _, e) -> 1 + size e
  | _ -> 1

(* インライン展開ルーチン本体 *)
let rec g env = function
  | IfEq (x, y, e1, e2) -> IfEq (x, y, g env e1, g env e2)
  | IfLE (x, y, e1, e2) -> IfLE (x, y, g env e1, g env e2)
  | IfEqZ (x, e1, e2) -> IfEqZ (x, g env e1, g env e2)
  | IfLEZ (x, e1, e2) -> IfLEZ (x, g env e1, g env e2)
  | IfGEZ (x, e1, e2) -> IfGEZ (x, g env e1, g env e2)
  | Let (xt, e1, e2) -> Let (xt, g env e1, g env e2)
  | LetRec ({ name = (x, t); args = yts; body = e1 }, e2) ->
      let env = if size e1 > !threshold then env else M.add x (yts, e1) env in
      LetRec ({ name = (x, t); args = yts; body = g env e1}, g env e2)
  | App (x, ys) when M.mem x env ->
      let (zs, e) = M.find x env in
      if !Typing.lv >= 2 then Format.eprintf "[info] inlining %s@." x;
      let env' = List.fold_left2
        (fun env' (z, t) y -> M.add z y env')
        M.empty zs ys in
      Alpha.g env' e
  | LetTuple (xts, y, e) -> LetTuple (xts, y, g env e)
  | e -> e

let f e = g M.empty e

