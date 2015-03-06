
open KNormal

let memi x env =
  try match M.find x env with
    | Int _ -> true
    | _ -> false
  with Not_found -> false

let memf x env =
  try match M.find x env with
    | Float _ -> true
    | _ -> false
  with Not_found -> false

let memt x env =
  try match M.find x env with
    | Tuple _ -> true
    | _ -> false
  with Not_found -> false

let findi x env =
  match M.find x env with
    | Int i -> i
    | _ -> raise Not_found

let findf x env =
  match M.find x env with
    | Float f -> f
    | _ -> raise Not_found

let findt x env =
  match M.find x env with
    | Tuple t -> t
    | _ -> raise Not_found

let sign_inv = function
  | Nop -> Inv
  | Plus -> Minus
  | Minus -> Plus
  | Inv -> Nop

let do_sign s f =
  match s with
   | Nop -> f
   | Plus -> abs_float f
   | Minus -> -. (abs_float f)
   | Inv -> -.f

(* 定数畳み込みルーチン本体 *)
let rec g env = function
  | Var x when memi x env -> Int (findi x env)
  | Not x as e ->
      begin try match M.find x env with
        | Int x -> Int (if x = 0 then 1 else 0)
        | Eq (x, y) -> Ne (x, y)
        | Ne (x, y) -> Eq (x, y)
        | Lt (x, y) -> Le (y, x)
        | Le (x, y) -> Lt (y, x)
        | FEq (x, y) -> FNe (x, y)
        | FNe (x, y) -> FEq (x, y)
        | FLt (x, y) -> FLe (y, x)
        | FLe (x, y) -> FLt (y, x)
        | _ -> e
      with Not_found -> e end
  | Neg x as e ->
      begin try match M.find x env with
        | Int x -> Int (-x)
        | Neg x -> Var x
        | Sub (x, V y) -> Sub (y, V x)
        | _ -> e
      with Not_found -> e end
  | Add (x, C y) as e ->
      begin try match M.find x env with
        | Int x -> Int (x + y)
        | Add (x, C z) -> Add (x, C (y + z))
        | Sub (x, C z) -> Add (x, C (y - z))
        | _ -> e
      with Not_found -> e end
  | Add (x, V y) when memi x env -> Add (y, C (findi x env))
  | Add (x, V y) when memi y env -> Add (x, C (findi y env))
  | Sub (x, C y) as e ->
      begin try match M.find x env with
        | Int x -> Int (x - y)
        | Add (x, C z) -> Sub (x, C (y - z))
        | Sub (x, C z) -> Sub (x, C (y + z))
        | _ -> e
      with Not_found -> e end
  | Sub (x, V y) when memi y env -> Sub (x, C (findi y env))
  | Shl (x, y) when memi x env -> Int (findi x env lsl y)
  | Shr (x, y) when memi x env -> Int (findi x env lsr y)
  | FNeg x as e ->
      begin try match M.find x env with
        | Float x -> Float (-.x)
        | FNeg x -> Var x
        | FAdd (s, x, y) -> FAdd (sign_inv s, x, y)
        | FSub (s, x, y) -> FSub (sign_inv s, x, y)
        | FMul (s, x, y) -> FMul (sign_inv s, x, y)
        | _ -> e
      with Not_found -> e end
  | FAbs x as e ->
      begin try match M.find x env with
        | Float x -> Float (abs_float x)
        | FNeg x | FAbs x -> FAbs x
        | FAdd (s, x, y) -> FAdd (Plus, x, y)
        | FSub (s, x, y) -> FSub (Plus, x, y)
        | FMul (s, x, y) -> FMul (Plus, x, y)
        | _ -> e
      with Not_found -> e end
  | FAdd (s, x, y) when memf x env && memf y env ->
      Float (do_sign s (findf x env +. findf y env))
  | FSub (s, x, y) when memf x env && memf y env ->
      Float (do_sign s (findf x env -. findf y env))
  | FMul (s, x, y) when memf x env && memf y env ->
      Float (do_sign s (findf x env *. findf y env))
  | Eq (x, y) when memi x env && memi y env ->
      Int (if findi x env =  findi y env then 1 else 0)
  | Ne (x, y) when memi x env && memi y env ->
      Int (if findi x env <> findi y env then 1 else 0)
  | Lt (x, y) when memi x env && memi y env ->
      Int (if findi x env <  findi y env then 1 else 0)
  | Le (x, y) when memi x env && memi y env ->
      Int (if findi x env <= findi y env then 1 else 0)
  | FEq (x, y) when memf x env && memf y env ->
      Int (if findf x env =  findf y env then 1 else 0)
  | FNe (x, y) when memf x env && memf y env ->
      Int (if findf x env <> findf y env then 1 else 0)
  | FLt (x, y) when memf x env && memf y env ->
      Int (if findf x env <  findf y env then 1 else 0)
  | FLe (x, y) when memf x env && memf y env ->
      Int (if findf x env <= findf y env then 1 else 0)
  | IToF x when memi x env -> Float (float (findi x env))
  | FToI x when memf x env -> Int (int_of_float (floor (findf x env +. 0.5)))
  | Floor x when memf x env -> Float (floor (findf x env))
  | IfEq (x, y, e1, e2) when memi x env && memi y env ->
      if findi x env = findi y env then g env e1 else g env e2
  | IfEq (x, y, e1, e2) when memi x env && findi x env = 0 ->
      IfZ (y, g env e1, g env e2)
  | IfEq (x, y, e1, e2) when memi y env && findi y env = 0 ->
      IfZ (x, g env e1, g env e2)
  | IfEq (x, y, e1, e2) ->
      IfEq (x, y, g env e1, g env e2)
  | IfNe (x, y, e1, e2) when memi x env && memi y env ->
      if findi x env <> findi y env then g env e1 else g env e2
  | IfNe (x, y, e1, e2) when memi x env && findi x env = 0 ->
      IfNz (y, g env e1, g env e2)
  | IfNe (x, y, e1, e2) when memi y env && findi y env = 0 ->
      IfNz (x, g env e1, g env e2)
  | IfNe (x, y, e1, e2) ->
      IfNe (x, y, g env e1, g env e2)
  | IfZ (x, e1, e2) ->
      begin try match M.find x env with
        | Int x -> if x = 0 then g env e1 else g env e2
        | Not x -> IfNz (x, g env e1, g env e2)
        | Eq (x, y) -> IfNe (x, y, g env e1, g env e2)
        | Ne (x, y) -> IfEq (x, y, g env e1, g env e2)
        | _ -> IfZ (x, g env e1, g env e2)
      with Not_found -> IfZ (x, g env e1, g env e2) end
  | IfNz (x, e1, e2) ->
      begin try match M.find x env with
        | Int x -> if x <> 0 then g env e1 else g env e2
        | Not x -> IfZ (x, g env e1, g env e2)
        | Eq (x, y) -> IfEq (x, y, g env e1, g env e2)
        | Ne (x, y) -> IfNe (x, y, g env e1, g env e2)
        | _ -> IfNz (x, g env e1, g env e2)
      with Not_found -> IfNz (x, g env e1, g env e2) end
  | Let ((x, t), e1, e2) ->
      let e1' = g env e1 in
      let e2' = g (M.add x e1' env) e2 in
      Let ((x, t), e1', e2')
  | LetRec ({ name = x; args = ys; body = e1 }, e2) ->
      LetRec({ name = x; args = ys; body = g env e1 }, g env e2)
  | LetTuple (xts, y, e) when memt y env ->
      List.fold_left2
        (fun e' xt z -> Let (xt, Var z, e'))
        (g env e)
        xts
        (findt y env)
  | LetTuple (xts, y, e) -> LetTuple (xts, y, g env e)
  | Load (x, y) as e ->
      begin try match M.find x env with
        | Add (x, C z) -> Load (x, y + z)
        | Sub (x, C z) -> Load (x, y - z)
        | ExtTuple x | ExtArray x -> LoadL (x, y)
        | _ -> e
      with Not_found -> e end
  | Store (x, y, z) as e ->
      begin try match M.find y env with
        | Add (y, C w) -> Store (x, y, z + w)
        | Sub (y, C w) -> Store (x, y, z - w)
        | ExtTuple y | ExtArray y -> StoreL (x, y, z)
        | _ -> e
      with Not_found -> e end
  | e -> e

let f = g M.empty

