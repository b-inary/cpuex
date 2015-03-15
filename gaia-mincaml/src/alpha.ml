
(* rename identifiers to make them unique (alpha-conversion) *)

open KNormal

let find x env = try M.find x env with Not_found -> x

(* α変換ルーチン本体 *)
let rec g env = function
  | Unit -> Unit
  | Int i -> Int i
  | Float d -> Float d
  | Not x -> Not (find x env)
  | Neg x -> Neg (find x env)
  | Add (x, V y) -> Add (find x env, V (find y env))
  | Add (x, C y) -> Add (find x env, C y)
  | Sub (x, V y) -> Sub (find x env, V (find y env))
  | Sub (x, C y) -> Sub (find x env, C y)
  | Shl (x, y) -> Shl (find x env, y)
  | Shr (x, y) -> Shr (find x env, y)
  | FNeg x -> FNeg (find x env)
  | FAbs x -> FAbs (find x env)
  | FInv x -> FInv (find x env)
  | Sqrt x -> Sqrt (find x env)
  | FAdd (s, x, y) -> FAdd (s, find x env, find y env)
  | FSub (s, x, y) -> FSub (s, find x env, find y env)
  | FMul (s, x, y) -> FMul (s, find x env, find y env)
  | Cmp (i, x, V y) -> Cmp (i, find x env, V (find y env))
  | Cmp (i, x, C y) -> Cmp (i, find x env, C y)
  | IToF x -> IToF (find x env)
  | FToI x -> FToI (find x env)
  | Floor x -> Floor (find x env)
  | IfEq (x, y, e1, e2) -> IfEq (find x env, find y env, g env e1, g env e2)
  | IfNe (x, y, e1, e2) -> IfNe (find x env, find y env, g env e1, g env e2)
  | IfZ (x, e1, e2) -> IfZ (find x env, g env e1, g env e2)
  | IfNz (x, e1, e2) -> IfNz (find x env, g env e1, g env e2)
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
  | LoadL (x, y) -> LoadL (x, y)
  | StoreL (x, y, z) -> StoreL (find x env, y, z)
  | ExtTuple x -> ExtTuple x
  | ExtArray x -> ExtArray x
  | ExtFunApp (x, ys) -> ExtFunApp (x, List.map (fun y -> find y env) ys)

let f = g M.empty

