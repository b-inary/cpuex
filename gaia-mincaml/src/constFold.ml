
open KNormal

let memi x env = try match M.find x env with Int _ -> true | _ -> false with Not_found -> false
let memf x env = try match M.find x env with Float _ -> true | _ -> false with Not_found -> false
let memt x env = try match M.find x env with Tuple _ -> true | _ -> false with Not_found -> false
let memnot x env = try match M.find x env with Not _ -> true | _ -> false with Not_found -> false
let memneg x env = try match M.find x env with Neg _ -> true | _ -> false with Not_found -> false
let memadd x env = try match M.find x env with Add (_, C _) -> true | _ -> false with Not_found -> false
let memsub x env = try match M.find x env with Sub (_, C _) -> true | _ -> false with Not_found -> false
let memfneg x env = try match M.find x env with FNeg _ -> true | _ -> false with Not_found -> false
let memfadd x env = try match M.find x env with FAdd _ -> true | _ -> false with Not_found -> false
let memfsub x env = try match M.find x env with FSub _ -> true | _ -> false with Not_found -> false
let memfmul x env = try match M.find x env with FMul _ -> true | _ -> false with Not_found -> false
let memeq x env = try match M.find x env with Eq _ -> true | _ -> false with Not_found -> false
let memne x env = try match M.find x env with Ne _ -> true | _ -> false with Not_found -> false
let memlabel x env = try match M.find x env with ExtTuple _ | ExtArray _ -> true | _ -> false with Not_found -> false

let findi x env = match M.find x env with Int i -> i | _ -> raise Not_found
let findf x env = match M.find x env with Float f -> f | _ -> raise Not_found
let findt x env = match M.find x env with Tuple t -> t | _ -> raise Not_found
let findnot x env = match M.find x env with Not x -> x | _ -> raise Not_found
let findneg x env = match M.find x env with Neg x -> x | _ -> raise Not_found
let findadd x env = match M.find x env with Add (a, C b) -> (a, b) | _ -> raise Not_found
let findsub x env = match M.find x env with Sub (a, C b) -> (a, b) | _ -> raise Not_found
let findfneg x env = match M.find x env with FNeg x -> x | _ -> raise Not_found
let findfadd x env = match M.find x env with FAdd (s, a, b) -> (s, a, b) | _ -> raise Not_found
let findfsub x env = match M.find x env with FSub (s, a, b) -> (s, a, b) | _ -> raise Not_found
let findfmul x env = match M.find x env with FMul (s, a, b) -> (s, a, b) | _ -> raise Not_found
let findeq x env = match M.find x env with Eq (a, b) -> (a, b) | _ -> raise Not_found
let findne x env = match M.find x env with Ne (a, b) -> (a, b) | _ -> raise Not_found
let findlabel x env = match M.find x env with ExtTuple x | ExtArray x -> x | _ -> raise Not_found

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
(* [b-inary] 浮動小数点数の畳み込みは怪しいけど良いのかな...? *)
let rec g env = function
  | Var x when memi x env -> Int (findi x env)
  | Var x when memf x env -> Float (findf x env)
  (* | Var(x) when memt x env -> Tuple (findt x env) *)
  | Neg x when memi x env -> Int (- (findi x env))
  | Add (x, C y) when memi x env -> Int (findi x env + y)
  | Add (x, C y) when memadd x env -> let a, b = findadd x env in Add (a, C ( b + y))
  | Add (x, C y) when memsub x env -> let a, b = findsub x env in Add (a, C (-b + y))
  | Add (x, V y) when memi x env -> Add (y, C (findi x env))
  | Add (x, V y) when memi y env -> Add (x, C (findi y env))
  | Sub (x, C y) when memi x env -> Int (findi x env - y)
  | Sub (x, C y) when memadd x env -> let a, b = findadd x env in Sub (a, C (-b + y))
  | Sub (x, C y) when memsub x env -> let a, b = findsub x env in Sub (a, C ( b + y))
  | Sub (x, V y) when memi x env -> Sub (y, C (findi x env))
  | Sub (x, V y) when memi y env -> Sub (x, C (findi y env))
  | Shl (x, y) when memi x env -> Int (findi x env lsl y)
  | Shr (x, y) when memi x env -> Int (findi x env lsr y)
  | FNeg x when memf x env -> Float (-. (findf x env))
  | FNeg x when memfadd x env -> let s, a, b = findfadd x env in FAdd (sign_inv s, a, b)
  | FNeg x when memfsub x env -> let s, a, b = findfsub x env in FSub (sign_inv s, a, b)
  | FNeg x when memfmul x env -> let s, a, b = findfmul x env in FMul (sign_inv s, a, b)
  | FAbs x when memf x env -> Float (abs_float (findf x env))
  | FAbs x when memfadd x env -> let s, a, b = findfadd x env in FAdd (Plus, a, b)
  | FAbs x when memfsub x env -> let s, a, b = findfsub x env in FSub (Plus, a, b)
  | FAbs x when memfmul x env -> let s, a, b = findfmul x env in FMul (Plus, a, b)
  | FAdd (s, x, y) when memf x env && memf y env -> Float (do_sign s (findf x env +. findf y env))
  | FSub (s, x, y) when memf x env && memf y env -> Float (do_sign s (findf x env -. findf y env))
  | FMul (s, x, y) when memf x env && memf y env -> Float (do_sign s (findf x env *. findf y env))
  | IfEq (x, y, e1, e2) when memi x env && memi y env -> if findi x env = findi y env then g env e1 else g env e2
  | IfEq (x, y, e1, e2) when memf x env && memf y env -> if findf x env = findf y env then g env e1 else g env e2
  | IfEq (x, y, e1, e2) when memi x env && findi x env = 0 -> IfZ (y, g env e1, g env e2)
  | IfEq (x, y, e1, e2) when memi y env && findi y env = 0 -> IfZ (x, g env e1, g env e2)
  | IfEq (x, y, e1, e2) -> IfEq (x, y, g env e1, g env e2)
  | IfNe (x, y, e1, e2) when memi x env && memi y env -> if findi x env <> findi y env then g env e1 else g env e2
  | IfNe (x, y, e1, e2) when memf x env && memf y env -> if findf x env <> findf y env then g env e1 else g env e2
  | IfNe (x, y, e1, e2) when memi x env && findi x env = 0 -> IfNz (y, g env e1, g env e2)
  | IfNe (x, y, e1, e2) when memi y env && findi y env = 0 -> IfNz (x, g env e1, g env e2)
  | IfNe (x, y, e1, e2) -> IfNe (x, y, g env e1, g env e2)
  | IfZ (x, e1, e2) when memi x env -> if findi x env = 0 then g env e1 else g env e2
  | IfZ (x, e1, e2) when memnot x env -> let a = findnot x env in IfZ (a, e2, e1)
  | IfZ (x, e1, e2) when memeq x env -> let a, b = findeq x env in IfNe (a, b, e1, e2)
  | IfZ (x, e1, e2) when memne x env -> let a, b = findne x env in IfEq (a, b, e1, e2)
  | IfZ (x, e1, e2) -> IfZ (x, g env e1, g env e2)
  | IfNz (x, e1, e2) when memi x env -> if findi x env = 0 then g env e2 else g env e1
  | IfNz (x, e1, e2) when memnot x env -> let a = findnot x env in IfNz (a, e2, e1)
  | IfNz (x, e1, e2) when memeq x env -> let a, b = findeq x env in IfEq (a, b, e1, e2)
  | IfNz (x, e1, e2) when memne x env -> let a, b = findne x env in IfNe (a, b, e1, e2)
  | IfNz (x, e1, e2) -> IfNz (x, g env e1, g env e2)
  | Let ((x, t), e1, e2) ->
      let e1' = g env e1 in
      let e2' = g (M.add x e1' env) e2 in
      Let ((x, t), e1', e2')
  | LetRec ({ name = x; args = ys; body = e1 }, e2) ->
      LetRec({ name = x; args = ys; body = g env e1 }, g env e2)
  | LetTuple (xts, y, e) when memt y env ->
      List.fold_left2 (fun e' xt z -> Let(xt, Var(z), e')) (g env e) xts (findt y env)
  | LetTuple (xts, y, e) -> LetTuple(xts, y, g env e)
  | Load (x, y) when memadd x env -> let a, b = findadd x env in Load (a, b + y)
  | Load (x, y) when memsub x env -> let a, b = findsub x env in Load (a, -b + y)
  | Load (x, y) when memlabel x env -> let l = findlabel x env in LoadL (l, y)
  | Store (x, y, z) when memadd y env -> let a, b = findadd y env in Store (x, a, b + z)
  | Store (x, y, z) when memsub y env -> let a, b = findsub y env in Store (x, a, -b + z)
  | Store (x, y, z) when memlabel y env -> let l = findlabel y env in StoreL (x, l, z)
  | e -> e

let f = g M.empty

