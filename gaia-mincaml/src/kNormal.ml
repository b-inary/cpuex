
(* give names to intermediate values (K-normalization) *)

type var_or_imm = V of Id.t | C of int
type fpu_sign = Nop | Plus | Minus | Inv

(* K正規化後の式 *)
type t =
  | Unit | Int of int | Float of float
  | Not of Id.t | Neg of Id.t
  | Add of Id.t * var_or_imm | Sub of Id.t * var_or_imm
  | Shl of Id.t * int | Shr of Id.t * int
  | FNeg of Id.t | FAbs of Id.t
  | FAdd of fpu_sign * Id.t * Id.t
  | FSub of fpu_sign * Id.t * Id.t
  | FMul of fpu_sign * Id.t * Id.t
  | Eq of Id.t * Id.t | Ne of Id.t * Id.t
  | Lt of Id.t * Id.t | Le of Id.t * Id.t
  | FEq of Id.t * Id.t | FNe of Id.t * Id.t
  | FLt of Id.t * Id.t | FLe of Id.t * Id.t
  | IToF of Id.t | FToI of Id.t | Floor of Id.t
  | IfEq of Id.t * Id.t * t * t | IfNe of Id.t * Id.t * t * t
  | IfZ of Id.t * t * t | IfNz of Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of Id.t * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Load of Id.t * int
  | Store of Id.t * Id.t * int
  | LoadL of Id.t * int
  | StoreL of Id.t * Id.t * int
  | ExtTuple of Id.t
  | ExtArray of Id.t
  | ExtFunApp of Id.t * Id.t list

and fundef =
  { name: Id.t * Type.t; args: (Id.t * Type.t) list; body: t }

let is_pow2 i = i <> 0 && (i land (i - 1)) = 0
let log2 i = let rec go n m = if m = i then n else go (n + 1) (m * 2) in go 0 1

(* 式に出現する自由変数 *)
let rec fv = function
  | Unit | Int _ | Float _ | LoadL _ | ExtTuple _ | ExtArray _ -> S.empty
  | Not x | Neg x | Add (x, C _) | Sub (x, C _) | Shl (x, _) | Shr (x, _)
  | FNeg x | FAbs x | IToF x | FToI x | Floor x | Load (x, _) | StoreL (x, _, _) -> S.singleton x
  | Add (x, V y) | Sub (x, V y) | FAdd (_, x, y) | FSub (_, x, y) | FMul (_, x, y)
  | Eq (x, y) | Ne (x, y) | Lt (x, y) | Le (x, y) | FEq (x, y) | FNe (x, y) | FLt (x, y) | FLe (x, y)
  | Store (x, y, _) -> S.of_list [x; y]
  | IfEq (x, y, e1, e2) | IfNe (x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | IfZ (x, e1, e2) | IfNz (x, e1, e2) -> S.add x (S.union (fv e1) (fv e2))
  | Let ((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var x -> S.singleton x
  | LetRec ({ name = (x, t); args = yts; body = e1 }, e2) ->
      let zs = S.diff (fv e1) (S.of_list (List.map fst yts)) in
      S.diff (S.union zs (fv e2)) (S.singleton x)
  | App (x, ys) -> S.of_list (x :: ys)
  | Tuple xs | ExtFunApp (_, xs) -> S.of_list xs
  | LetTuple (xs, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xs)))

(* letを挿入する補助関数 *)
let insert_let (e, t) k =
  match e with
    | Var x -> k x
    | _ ->
        let x = Id.gentmp t in
        let (e', t') = k x in
        (Let ((x, t), e, e'), t')

(* K正規化ルーチン本体 *)
let rec g env = function
  | Syntax.Unit -> (Unit, Type.Unit)
  | Syntax.Bool b -> (Int (if b then 1 else 0), Type.Int)
  | Syntax.Int i -> (Int i, Type.Int)
  | Syntax.Float d -> (Float d, Type.Float)
  | Syntax.Not e -> insert_let (g env e) (fun x -> (Not x, Type.Int))
  | Syntax.Neg e -> insert_let (g env e) (fun x -> (Neg x, Type.Int))
  | Syntax.Add (e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2) (fun y -> (Add (x, V y), Type.Int)))
  | Syntax.Sub (e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2) (fun y -> (Sub (x, V y), Type.Int)))
  | Syntax.Mul (e1, Syntax.Int i) ->
      if is_pow2 i then insert_let (g env e1) (fun x -> (Shl (x, (log2 i)), Type.Int)) else
      failwith (Printf.sprintf "error: multiply: %d is not power of 2" i)
  | Syntax.Mul (Syntax.Int i, e2) -> g env (Syntax.Mul (e2, Syntax.Int i))
  | Syntax.Mul _ -> failwith ("error: cannot multiply two variables")
  | Syntax.Div (e1, Syntax.Int i) ->
      if is_pow2 i then insert_let (g env e1) (fun x -> (Shr (x, (log2 i)), Type.Int)) else
      failwith (Printf.sprintf "error: divide: %d is not power of 2" i)
  | Syntax.Div _ -> failwith ("error: cannot divide by variable")
  | Syntax.FNeg e -> insert_let (g env e) (fun x -> (FNeg x, Type.Float))
  | Syntax.FAbs e -> insert_let (g env e) (fun x -> (FAbs x, Type.Float))
  | Syntax.FAdd (e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2) (fun y -> (FAdd (Nop, x, y), Type.Float)))
  | Syntax.FSub (e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2) (fun y -> (FSub (Nop, x, y), Type.Float)))
  | Syntax.FMul (e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2) (fun y -> (FMul (Nop, x, y), Type.Float)))
  | Syntax.Eq (e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2) (fun y -> (Eq (x, y), Type.Int)))
  | Syntax.Ne (e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2) (fun y -> (Ne (x, y), Type.Int)))
  | Syntax.Lt (e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2) (fun y -> (Lt (x, y), Type.Int)))
  | Syntax.Le (e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2) (fun y -> (Le (x, y), Type.Int)))
  | Syntax.FEq (e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2) (fun y -> (FEq (x, y), Type.Int)))
  | Syntax.FNe (e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2) (fun y -> (FNe (x, y), Type.Int)))
  | Syntax.FLt (e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2) (fun y -> (FLt (x, y), Type.Int)))
  | Syntax.FLe (e1, e2) ->
      insert_let (g env e1)
        (fun x -> insert_let (g env e2) (fun y -> (FLe (x, y), Type.Int)))
  | Syntax.IToF e -> insert_let (g env e) (fun x -> (IToF x, Type.Float))
  | Syntax.FToI e -> insert_let (g env e) (fun x -> (FToI x, Type.Int))
  | Syntax.Floor e -> insert_let (g env e) (fun x -> (Floor x, Type.Float))
  | Syntax.If (e1, e2, e3) ->
      insert_let (g env e1)
        (fun x ->
          let e2', t2 = g env e2 in
          let e3', t3 = g env e3 in
          IfNz (x, e2', e3'), t2)
  | Syntax.Let ((x, t), e1, e2) ->
      let e1', t1 = g env e1 in
      let e2', t2 = g (M.add x t env) e2 in
      (Let ((x, t), e1', e2'), t2)
  | Syntax.Var x when M.mem x env -> (Var x, M.find x env)
  | Syntax.Var x ->
      (match M.find x !Typing.extenv with
        | Type.Tuple _ as t -> (ExtTuple x, t)
        | Type.Array _ as t -> (ExtArray x, t)
        | _ -> failwith (Printf.sprintf "external variable %s is not a tuple or an array" x))
  | Syntax.LetRec ({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1 }, e2) ->
      let env' = M.add x t env in
      let e2', t2 = g env' e2 in
      let e1', t1 = g (M.add_list yts env') e1 in
      (LetRec ({ name = (x, t); args = yts; body = e1' }, e2'), t2)
  | Syntax.App (Syntax.Var f, e2s) when not (M.mem f env) ->
      (match M.find f !Typing.extenv with
        | Type.Fun (_, t) ->
            let rec bind xs = function
              | [] -> (ExtFunApp (f, xs), t)
              | e2 :: e2s ->
                  insert_let (g env e2) (fun x -> bind (xs @ [x]) e2s) in
            bind [] e2s
        | _ -> assert false)
  | Syntax.App (e1, e2s) ->
      (match g env e1 with
        | (_, Type.Fun (_, t)) as g_e1 ->
            insert_let g_e1
              (fun f ->
                let rec bind xs = function
                  | [] -> App(f, xs), t
                  | e2 :: e2s ->
                      insert_let (g env e2) (fun x -> bind (xs @ [x]) e2s) in
                bind [] e2s)
        | _ -> assert false)
  | Syntax.Tuple es ->
      let rec bind xs ts = function
        | [] -> (Tuple xs, Type.Tuple ts)
        | e :: es ->
            let (_, t) as g_e = g env e in
            insert_let g_e (fun x -> bind (xs @ [x]) (ts @ [t]) es) in
      bind [] [] es
  | Syntax.LetTuple (xts, e1, e2) ->
      insert_let (g env e1)
        (fun y -> let e2', t2 = g (M.add_list xts env) e2 in
                  (LetTuple (xts, y, e2'), t2))
  | Syntax.Array (e1, e2) ->
      insert_let (g env e1)
        (fun x ->
          let (_, t2) as g_e2 = g env e2 in
          insert_let g_e2 (fun y -> (ExtFunApp ("create_array", [x; y]), Type.Array t2)))
  | Syntax.Get (e1, e2) ->
      (match g env e1 with
        | (_, Type.Array t) ->
            insert_let (g env (Syntax.Add (e1, Syntax.Mul (e2, Syntax.Int 4)))) (fun x -> (Load (x, 0), t))
        | _ -> assert false)
  | Syntax.Put (e1, e2, e3) ->
      insert_let (g env (Syntax.Add (e1, Syntax.Mul (e2, Syntax.Int 4))))
        (fun x -> insert_let (g env e3) (fun y -> (Store (y, x, 0), Type.Unit)))

let f e = fst (g M.empty e)

