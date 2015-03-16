
open Asm

let expand xts ini it =
  List.fold_left
    (fun (off, acc) (x, t) ->
      match t with
        | Type.Unit  -> (off, acc)
        | _ -> (off + 4, it x t off acc))
    ini xts

let sign_conv = function
  | Closure.Nop -> (Nop : fpu_sign)
  | Closure.Plus -> Plus
  | Closure.Minus -> Minus
  | Closure.Inv -> Inv

(* 式の仮想マシンコード生成 *)
let rec g env = function
  | Closure.Unit -> Ans Nop
  | Closure.Int i -> Ans (Li i)
  | Closure.Float f -> Ans (Lf f)
  | Closure.Not x -> Ans (Not x)
  | Closure.Neg x -> Ans (Neg x)
  | Closure.Add (x, Closure.V y) -> Ans (Add (x, V y))
  | Closure.Add (x, Closure.C y) -> Ans (Add (x, C y))
  | Closure.Sub (x, Closure.V y) -> Ans (Sub (x, V y))
  | Closure.Sub (x, Closure.C y) -> Ans (Sub (x, C y))
  | Closure.AddA (x, y) -> Ans (AddA (x, y))
  | Closure.Shl (x, y) -> Ans (Shl (x, y))
  | Closure.Shr (x, y) -> Ans (Shr (x, y))
  | Closure.FNeg x -> Ans (FNeg x)
  | Closure.FAbs x -> Ans (FAbs x)
  | Closure.FInv x -> Ans (FInv x)
  | Closure.Sqrt x -> Ans (Sqrt x)
  | Closure.FAdd (s, x, y) -> Ans (FAdd (sign_conv s, x, y))
  | Closure.FSub (s, x, y) -> Ans (FSub (sign_conv s, x, y))
  | Closure.FMul (s, x, y) -> Ans (FMul (sign_conv s, x, y))
  | Closure.Cmp (i, x, Closure.V y) -> Ans (Cmp (i, x, V y))
  | Closure.Cmp (i, x, Closure.C y) -> Ans (Cmp (i, x, C y))
  | Closure.IToF x -> Ans (IToF x)
  | Closure.FToI x -> Ans (FToI x)
  | Closure.Floor x -> Ans (Floor x)
  | Closure.IfEq (x, y, e1, e2) ->
      begin match M.find x env with
        | Type.Bool | Type.Int | Type.Float -> Ans (IfEq (x, y, g env e1, g env e2))
        | _ -> failwith "equality supported only for bool, int, and float"
      end
  | Closure.IfNe (x, y, e1, e2) ->
      begin match M.find x env with
        | Type.Bool | Type.Int | Type.Float -> Ans (IfNe (x, y, g env e1, g env e2))
        | _ -> failwith "equality supported only for bool, int, and float"
      end
  | Closure.IfZ (x, e1, e2) ->
      begin match M.find x env with
        | Type.Bool | Type.Int | Type.Float -> Ans (IfEq (x, "$0", g env e1, g env e2))
        | _ -> failwith "equality supported only for bool, int, and float"
      end
  | Closure.IfNz (x, e1, e2) ->
      begin match M.find x env with
        | Type.Bool | Type.Int | Type.Float -> Ans (IfNe (x, "$0", g env e1, g env e2))
        | _ -> failwith "equality supported only for bool, int, and float"
      end
  | Closure.Let ((x, t1), e1, e2) ->
      let e1' = g env e1 in
      let e2' = g (M.add x t1 env) e2 in
      concat e1' (x, t1) e2'
  | Closure.Var x ->
      (match M.find x env with
        | Type.Unit -> Ans Nop
        | _ -> Ans (Mov x))
  | Closure.MakeCls ((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2) ->
      failwith "closure.makecls"
(*
      let e2' = g (M.add x t env) e2 in
      let (offset, store_fv) =
        expand (List.map (fun y -> (y, M.find y env)) ys) (1, e2')
               (fun y _ offset store_fv -> seq (St (y, x, offset), store_fv)) in
      let y = Id.genid "l" in
      let z = Id.genid "l" in
      Let ((x, t), Ld ("$0", heap_addr),
      Let ((y, Type.Int), Addi (x, offset),
      seq (St (y, "$0", heap_addr),
      Let ((z, Type.Int), MovL l, seq (St (z, x, 0), store_fv)))))
 *)
  | Closure.AppCls (x, ys) -> Ans (CallCls (x, ys))
  | Closure.AppDir (x, ys) -> Ans (CallDir (x, ys))
  | Closure.Tuple xs ->
      let y = Id.genid "t" in
      let z = Id.genid "t" in
      let (offset, store) =
        expand
          (List.map (fun x -> (x, M.find x env)) xs)
          (0, Ans (Mov y))
          (fun x _ offset store -> seq (St (x, y, offset), store)) in
      Let ((y, Type.Tuple (List.map (fun x -> M.find x env) xs)), LdL (Id.L "heap_ptr", 0),
      Let ((z, Type.Int), Add (y, C offset),
      seq (StL (z, Id.L "heap_ptr", 0), store)))
  | Closure.LetTuple (xts, y, e2) ->
      let s = Closure.fv e2 in
      let (offset, load) =
        expand
          xts
          (0, g (M.add_list xts env) e2)
          (fun x t offset load ->
            if not (S.mem x s) then load else
            Let ((x, t), Ld (y, offset), load)) in
      load
  | Closure.Load (x, y) -> Ans (Ld (x, y))
  | Closure.Store (x, y, z) -> Ans (St (x, y, z))
  | Closure.LoadL (x, y) -> Ans (LdL (x, y))
  | Closure.StoreL (x, y, z) -> Ans (StL (x, y, z))
  | Closure.ExtTuple (Id.L x) -> Ans (MovL (Id.L x))
  | Closure.ExtArray (Id.L x) -> Ans (MovL (Id.L x))

(* 関数の仮想マシンコード生成 *)
let h { Closure.name = (Id.L x, t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e } =
  let e = g (M.add x t (M.add_list yts (M.add_list zts M.empty))) e in
  match t with
    | Type.Fun (_, t2) -> { name = Id.L x; args = fst (List.split yts); body = e; ret = t2 }
    | _ -> assert false

(* プログラム全体の仮想マシンコード生成 *)
let f (Closure.Prog (fundefs, e)) =
  let fundefs = List.map h fundefs in
  let e = g M.empty e in
  Prog (fundefs, e)

