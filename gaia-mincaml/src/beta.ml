
open KNormal

(* 置換のための関数 *)
let find x env = try M.find x env with Not_found -> x

(* β簡約ルーチン本体 *)
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
  | Eq (x, y) -> Eq (find x env, find y env)
  | Ne (x, y) -> Ne (find x env, find y env)
  | Lt (x, y) -> Lt (find x env, find y env)
  | Le (x, y) -> Le (find x env, find y env)
  | FLt (x, y) -> FLt (find x env, find y env)
  | FLe (x, y) -> FLe (find x env, find y env)
  | IToF x -> IToF (find x env)
  | FToI x -> FToI (find x env)
  | Floor x -> Floor (find x env)
  | IfEq (x, y, e1, e2) -> IfEq (find x env, find y env, g env e1, g env e2)
  | IfNe (x, y, e1, e2) -> IfNe (find x env, find y env, g env e1, g env e2)
  | IfZ (x, e1, e2) -> IfZ (find x env, g env e1, g env e2)
  | IfNz (x, e1, e2) -> IfNz (find x env, g env e1, g env e2)
  | Let ((x, t), e1, e2) ->
      (match g env e1 with
        | Var(y) ->
            if !Typing.lv >= 2 then Format.eprintf "[info] beta-reducing %s = %s@." x y;
            g (M.add x y env) e2
        | e1' -> let e2' = g env e2 in Let((x, t), e1', e2'))
  | LetRec ({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec ({ name = xt; args = yts; body = g env e1 }, g env e2)
  | Var x -> Var (find x env)
  | App (g, xs) -> App(find g env, List.map (fun x -> find x env) xs)
  | Tuple xs -> Tuple (List.map (fun x -> find x env) xs)
  | LetTuple (xts, y, e) -> LetTuple (xts, find y env, g env e)
  | Load (x, y) -> Load (find x env, y)
  | Store (x, y, z) -> Store (find x env, find y env, z)
  | LoadL (x, y) -> LoadL (x, y)
  | StoreL (x, y, z) -> StoreL (find x env, y, z)
  | ExtTuple x -> ExtTuple x
  | ExtArray x -> ExtArray x
  | ExtFunApp (x, ys) -> ExtFunApp(x, List.map (fun y -> find y env) ys)

let f = g M.empty

