
open Asm

(* 定数テーブル *)
let idata = ref []
let fdata = ref []

let expand xts ini it =
  List.fold_left
    (fun (off, acc) (x, t) ->
      match t with
        | Type.Unit  -> (off, acc)
        | _ -> (off + 1, it x t off acc))
    ini xts

let in_range x = -0x8000 <= x && x <= 0x7fff

let add_int_data i =
  try fst (List.find (fun (_, i') -> i = i') !idata)
  with Not_found ->
    let l = Id.L (Id.genid "int") in
    idata := (l, i) :: !idata; l

let add_float_data f =
  try fst (List.find (fun (_, f') -> f = f') !fdata)
  with Not_found ->
    let l = Id.L (Id.genid "flt") in
    fdata := (l, f) :: !fdata; l

(* 式の仮想マシンコード生成 *)
let rec g env = function
  | Closure.Unit -> Ans Nop
  | Closure.Int i when in_range i -> Ans (Li i)
  | Closure.Int i -> let l = add_int_data i in Ans (LdL l)
  | Closure.Float f -> let l = add_float_data f in Ans (LdL l)
  | Closure.Neg x -> Ans (Neg x)
  | Closure.Add (x, y) -> Ans (Add (x, y))
  | Closure.Addi (x, y) when in_range y -> Ans (Addi (x, y))
  | Closure.Addi (x, y) ->
      let l = add_int_data y in
      let t = Id.genid "t" in
      Let ((t, Type.Int), LdL l, Ans (Add (x, t)))
  | Closure.Add4 (x, y, z) when in_range z -> Ans (Add4 (x, y, z))
  | Closure.Add4 (x, y, z) ->
      let l = add_int_data z in
      let t = Id.genid "t" in
      let s = Id.genid "t" in
      Let ((t, Type.Int), LdL l, Let ((s, Type.Int), Add (x, y), Ans (Add (t, s))))
  | Closure.Sub (x, y) -> Ans (Sub (x, y))
  | Closure.Shift (x, y) -> Ans (Shift (x, y))
  | Closure.FNeg x -> Ans (FNeg x)
  | Closure.FAbs x -> Ans (FAbs x)
  | Closure.FAdd (x, y) -> Ans (FAdd (x, y))
  | Closure.FMul (x, y) -> Ans (FMul (x, y))
  | Closure.IfEq (x, y, e1, e2) ->
      (match M.find x env with
        | Type.Bool | Type.Int | Type.Float -> Ans (IfEq (x, y, g env e1, g env e2))
        | _ -> failwith "equality supported only for bool, int, and float")
  | Closure.IfLE (x, y, e1, e2) ->
      (match M.find x env with
        | Type.Bool | Type.Int | Type.Float -> Ans (IfLE (x, y, g env e1, g env e2))
        | _ -> failwith "inequality supported only for bool, int, and float")
  | Closure.IfEqZ (x, e1, e2) ->
      (match M.find x env with
        | Type.Bool | Type.Int | Type.Float -> Ans (IfEq (x, "$0", g env e1, g env e2))
        | _ -> failwith "equality supported only for bool, int, and float")
  | Closure.IfLEZ (x, e1, e2) ->
      (match M.find x env with
        | Type.Bool | Type.Int | Type.Float -> Ans (IfLE (x, "$0", g env e1, g env e2))
        | _ -> failwith "equality supported only for bool, int, and float")
  | Closure.IfGEZ (x, e1, e2) ->
      (match M.find x env with
        | Type.Bool | Type.Int | Type.Float -> Ans (IfLE ("$0", x, g env e1, g env e2))
        | _ -> failwith "equality supported only for bool, int, and float")
  | Closure.Let ((x, t1), e1, e2) ->
      let e1' = g env e1 in
      let e2' = g (M.add x t1 env) e2 in
      concat e1' (x, t1) e2'
  | Closure.Var x ->
      (match M.find x env with
        | Type.Unit -> Ans Nop
        | _ -> Ans (Mov x))
  | Closure.MakeCls ((x, t), { Closure.entry = l; Closure.actual_fv = ys }, e2) ->
      let e2' = g (M.add x t env) e2 in
      let (offset, store_fv) =
        expand (List.map (fun y -> (y, M.find y env)) ys) (1, e2')
               (fun y _ offset store_fv -> seq (St (y, x, offset), store_fv)) in
      let y = Id.genid "l" in
      let z = Id.genid "l" in
      Let ((x, t), Ld ("$0", 0x4000),
      Let ((y, Type.Int), Addi (x, offset),
      seq (St (y, "$0", 0x4000),
      Let ((z, Type.Int), MovL l, seq (St (z, x, 0), store_fv)))))
  | Closure.AppCls (x, ys) -> Ans (CallCls (x, ys))
  | Closure.AppDir (x, ys) -> Ans (CallDir (x, ys))
  | Closure.Tuple xs ->
      let y = Id.genid "t" in
      let z = Id.genid "t" in
      let (offset, store) =
        expand (List.map (fun x -> (x, M.find x env)) xs) (0, Ans (Mov y))
               (fun x _ offset store -> seq (St (x, y, offset), store)) in
      Let ((y, Type.Tuple (List.map (fun x -> M.find x env) xs)), Ld ("$0", 0x4000),
      Let ((z, Type.Int), Addi (y, offset),
      seq (St (z, "$0", 0x4000), store)))
  | Closure.LetTuple (xts, y, e2) ->
      let s = Closure.fv e2 in
      let (offset, load) =
        expand xts (0, g (M.add_list xts env) e2)
               (fun x t offset load ->
                  if not (S.mem x s) then load else
                  Let ((x, t), Ld (y, offset), load)) in
      load
  | Closure.Load (x, y) when in_range y -> Ans (Ld (x, y))
  | Closure.Load (x, y) ->
      let l = add_int_data y in
      let t = Id.genid "t" in
      let s = Id.genid "t" in
      Let ((t, Type.Int), LdL l, Let ((s, Type.Int), Add (x, t), Ans (Ld (s, 0))))
  | Closure.Store (x, y, z) when in_range z -> Ans (St (x, y, z))
  | Closure.Store (x, y, z) ->
      let l = add_int_data z in
      let t = Id.genid "t" in
      let s = Id.genid "t" in
      Let ((t, Type.Int), LdL l, Let ((s, Type.Int), Add (y, t), Ans (St (x, s, 0))))
  | Closure.ExtTuple (Id.L x) -> Ans (MovL (Id.L x))
  | Closure.ExtArray (Id.L x) -> Ans (MovL (Id.L x))

(* 関数の仮想マシンコード生成 *)
let h { Closure.name = (Id.L x, t); Closure.args = yts; Closure.formal_fv = zts; Closure.body = e } =
  let (offset, load) =
    expand zts (1, g (M.add x t (M.add_list yts (M.add_list zts M.empty))) e)
           (fun z t offset load -> Let ((z, t), Ld (reg_cl, offset), load)) in
  match t with
    | Type.Fun (_, t2) -> { name = Id.L x; args = fst (List.split yts); body = load; ret = t2 }
    | _ -> assert false

(* プログラム全体の仮想マシンコード生成 *)
let f (Closure.Prog (fundefs, e)) =
  idata := []; fdata := [];
  let fundefs = List.map h fundefs in
  let e = g M.empty e in
  Prog ((!idata, !fdata), fundefs, e)

