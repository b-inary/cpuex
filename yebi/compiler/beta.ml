
open KNormal

(* 置換のための関数 *)
let find x env = try M.find x env with Not_found -> x

(* β簡約ルーチン本体 *)
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
      (match g env e1 with
        | Var(y) ->
            if !Typing.lv >= 2 then Format.eprintf "[info] beta-reducing %s = %s@." x y;
            g (M.add x y env) e2
        | e1' -> let e2' = g env e2 in Let((x, t), e1', e2'))
  | LetRec ({ name = xt; args = yts; body = e1 }, e2) ->
      LetRec ({ name = xt; args = yts; body = g env e1 }, g env e2)
  | Var x -> Var (find x env)
  | Tuple xs -> Tuple (List.map (fun x -> find x env) xs)
  | LetTuple (xts, y, e) -> LetTuple (xts, find y env, g env e)
  | Load (x, y) -> Load (find x env, y)
  | Store (x, y, z) -> Store (find x env, find y env, z)
  | App (g, xs) -> App(find g env, List.map (fun x -> find x env) xs)
  | ExtTuple x -> ExtTuple x
  | ExtArray x -> ExtArray x
  | ExtFunApp (x, ys) -> ExtFunApp(x, List.map (fun y -> find y env) ys)

let f = g M.empty

