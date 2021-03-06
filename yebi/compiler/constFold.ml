
open KNormal

let memi x env =
  try (match M.find x env with Int _ -> true | _ -> false)
  with Not_found -> false
let memf x env =
  try (match M.find x env with Float _ -> true | _ -> false)
  with Not_found -> false
let memt x env =
  try (match M.find x env with Tuple _ -> true | _ -> false)
  with Not_found -> false
let memadd x env =
  try (match M.find x env with Add _ -> true | _ -> false)
  with Not_found -> false
let memaddi x env =
  try (match M.find x env with Addi _ -> true | _ -> false)
  with Not_found -> false
let memadd4 x env =
  try (match M.find x env with Add4 _ -> true | _ -> false)
  with Not_found -> false
let memneg x env =
  try (match M.find x env with Neg _ -> true | _ -> false)
  with Not_found -> false
let memfneg x env =
  try (match M.find x env with FNeg _ -> true | _ -> false)
  with Not_found -> false

let findi x env = (match M.find x env with Int   i -> i | _ -> raise Not_found)
let findf x env = (match M.find x env with Float d -> d | _ -> raise Not_found)
let findt x env = (match M.find x env with Tuple ys -> ys | _ -> raise Not_found)
let findadd  x env = (match M.find x env with Add  (a, b) -> (a, b) | _ -> raise Not_found)
let findaddi x env = (match M.find x env with Addi (a, b) -> (a, b) | _ -> raise Not_found)
let findadd4 x env = (match M.find x env with Add4 (a, b, c) -> (a, b, c) | _ -> raise Not_found)
let findneg x env = (match M.find x env with Neg x -> x | _ -> raise Not_found)
let findfneg x env = (match M.find x env with FNeg x -> x | _ -> raise Not_found)

(* 定数畳み込みルーチン本体 *)
(* [b-inary] 浮動小数点数の畳み込みは怪しいけど良いのかな...? *)
let rec g env = function
  | Var x  when memi x env -> Int (findi x env)
  (* | Var(x) when memf x env -> Float (findf x env) *)
  (* | Var(x) when memt x env -> Tuple (findt x env) *)
  | Neg x when memi x env -> Int (- (findi x env))
  | Add  (x, y) when memi x env && memi y env -> Int (findi x env + findi y env)
  | Addi (x, y) when memi x env -> Int (findi x env + y)
  | Add  (x, y) when memi x env -> Addi (y, findi x env)
  | Add  (x, y) when memi y env -> Addi (x, findi y env)
  | Add4 (x, y, z) when memi x env -> Addi (y, findi x env + z)
  | Add4 (x, y, z) when memi y env -> Addi (x, findi y env + z)
  | Add  (x, y) when memaddi x env -> let (a, b) = findaddi x env in Add4 (a, y, b)
  | Add  (x, y) when memaddi y env -> let (a, b) = findaddi y env in Add4 (x, a, b)
  | Addi (x, y) when memadd  x env -> let (a, b) = findadd  x env in Add4 (a, b, y)
  | Addi (x, y) when memaddi x env -> let (a, b) = findaddi x env in Addi (a, b + y)
  | Addi (x, y) when memadd4 x env -> let (a, b, c) = findadd4 x env in Add4 (a, b, c + y)
  | Add4 (x, y, z) when memaddi x env -> let (a, b) = findaddi x env in Add4 (a, y, b + z)
  | Add4 (x, y, z) when memaddi y env -> let (a, b) = findaddi y env in Add4 (x, a, b + z)
  | Sub (x, y) when memi x env && memi y env -> Int (findi x env - findi y env)
  | Sub (x, y) when memi y env -> Addi (x, - (findi y env))
  | Shift (x, y) when memi x env && findi x env > 0 -> Int ((findi x env) lsl y)
  | Shift (x, y) when memi x env                    -> Int ((findi x env) lsr (-y))
  | FNeg x when memf x env -> Float (-. (findf x env))
  | FAbs x when memf x env -> Float (abs_float (findf x env))
  | FAdd (x, y) when memf x env && memf y env -> Float (findf x env +. findf y env)
  | FAdd (x, y) when memf x env && findf x env = 0.0 -> Var y
  | FAdd (x, y) when memf y env && findf y env = 0.0 -> Var x
  | FMul (x, y) when memf x env && memf y env -> Float (findf x env *. findf y env)
  | FMul (x, y) when memf x env && findf x env = 1.0 -> Var y
  | FMul (x, y) when memf y env && findf y env = 1.0 -> Var x
  | IfEq (x, y, e1, e2) when memi x env && memi y env -> if findi x env = findi y env then g env e1 else g env e2
  | IfEq (x, y, e1, e2) when memf x env && memf y env -> if findf x env = findf y env then g env e1 else g env e2
  | IfEq (x, y, e1, e2) when (memi x env && findi x env = 0) || (memf x env && findf x env = 0.0) -> IfEqZ (y, g env e1, g env e2)
  | IfEq (x, y, e1, e2) when (memi y env && findi y env = 0) || (memf y env && findf y env = 0.0) -> IfEqZ (x, g env e1, g env e2)
  | IfEq (x, y, e1, e2) -> IfEq (x, y, g env e1, g env e2)
  | IfLE (x, y, e1, e2) when memi x env && memi y env -> if findi x env <= findi y env then g env e1 else g env e2
  | IfLE (x, y, e1, e2) when memf x env && memf y env -> if findf x env <= findf y env then g env e1 else g env e2
  | IfLE (x, y, e1, e2) when (memi x env && findi x env = 0) || (memf x env && findf x env = 0.0) -> IfGEZ (y, g env e1, g env e2)
  | IfLE (x, y, e1, e2) when (memi y env && findi y env = 0) || (memf y env && findf y env = 0.0) -> IfLEZ (x, g env e1, g env e2)
  | IfLE (x, y, e1, e2) -> IfLE (x, y, g env e1, g env e2)
  | IfEqZ (x, e1, e2) -> IfEqZ (x, g env e1, g env e2)
  | IfLEZ (x, e1, e2) when memneg x env -> IfGEZ (findneg x env, g env e1, g env e2)
  | IfLEZ (x, e1, e2) when memfneg x env -> IfGEZ (findfneg x env, g env e1, g env e2)
  | IfLEZ (x, e1, e2) -> IfLEZ (x, g env e1, g env e2)
  | IfGEZ (x, e1, e2) when memneg x env -> IfLEZ (findneg x env, g env e1, g env e2)
  | IfGEZ (x, e1, e2) when memfneg x env -> IfLEZ (findfneg x env, g env e1, g env e2)
  | IfGEZ (x, e1, e2) -> IfGEZ (x, g env e1, g env e2)
  | Let ((x, t), e1, e2) ->
      let e1' = g env e1 in
      let e2' = g (M.add x e1' env) e2 in
      Let ((x, t), e1', e2')
  | LetRec ({ name = x; args = ys; body = e1 }, e2) ->
      LetRec({ name = x; args = ys; body = g env e1 }, g env e2)
  | LetTuple (xts, y, e) when memt y env ->
      List.fold_left2 (fun e' xt z -> Let(xt, Var(z), e')) (g env e) xts (findt y env)
  | LetTuple (xts, y, e) -> LetTuple(xts, y, g env e)
  | Load (x, y) when memaddi x env -> let (a, b) = findaddi x env in Load (a, b + y)
  | Store (x, y, z) when memaddi y env -> let (a, b) = findaddi y env in Store (x, a, b + z)
  | e -> e

let f = g M.empty

