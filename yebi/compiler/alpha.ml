
(* rename identifiers to make them unique (alpha-conversion) *)

open KNormal

let find x env = try M.find x env with Not_found -> x

(* α変換ルーチン本体 *)
let rec g env = function
  | Unit -> Unit
  | Int   i -> Int i
  | Float d -> Float d
  | Neg x -> Neg (find x env)
  | Add   (x, y)    -> Add   (find x env, find y env)
  | Addi  (x, y)    -> Addi  (find x env, y)
  | Add4  (x, y, z) -> Add4  (find x env, find y env, z)
  | Sub   (x, y)    -> Sub   (find x env, find y env)
  | Shift (x, y)    -> Shift (find x env, y)
  | FNeg x      -> FNeg (find x env)
  | FAbs x      -> FAbs (find x env)
  | FAdd (x, y) -> FAdd (find x env, find y env)
  | FMul (x, y) -> FMul (find x env, find y env)
  | IfEq (x, y, e1, e2) -> IfEq (find x env, find y env, g env e1, g env e2)
  | IfLE (x, y, e1, e2) -> IfLE (find x env, find y env, g env e1, g env e2)
  | IfEqZ (x, e1, e2) -> IfEqZ (find x env, g env e1, g env e2)
  | IfLEZ (x, e1, e2) -> IfLEZ (find x env, g env e1, g env e2)
  | IfGEZ (x, e1, e2) -> IfGEZ (find x env, g env e1, g env e2)
  | Let ((x, t), e1, e2) ->
      let x' = Id.genid x in
      Let ((x', t), g env e1, g (M.add x x' env) e2)
  | Var x -> Var (find x env)
  | LetRec ({ name = (x, t); args = yts; body = e1 }, e2) ->
      let env = M.add x (Id.genid x) env in
      let ys = List.map fst yts in
      let env' = M.add_list2 ys (List.map Id.genid ys) env in
      LetRec ({ name = (find x env, t);
                args = List.map (fun (y, t) -> (find y env', t)) yts;
                body = g env' e1 },
              g env e2)
  | App (x, ys) -> App (find x env, List.map (fun y -> find y env) ys)
  | Tuple xs -> Tuple (List.map (fun x -> find x env) xs)
  | LetTuple (xts, y, e) ->
      let xs = List.map fst xts in
      let env' = M.add_list2 xs (List.map Id.genid xs) env in
      LetTuple (List.map (fun (x, t) -> (find x env', t)) xts, find y env, g env' e)
  | Load (x, y) -> Load (find x env, y)
  | Store (x, y, z) -> Store (find x env, find y env, z)
  | ExtTuple x -> ExtTuple x
  | ExtArray x -> ExtArray x
  | ExtFunApp (x, ys) -> ExtFunApp (x, List.map (fun y -> find y env) ys)

let f = g M.empty

