
type closure = { entry: Id.l; actual_fv: Id.t list }

type var_or_imm = V of Id.t | C of int
type fpu_sign = Nop | Plus | Minus | Inv

(* クロージャ変換後の式 *)
type t =
  | Unit | Int of int | Float of float
  | Not of Id.t | Neg of Id.t
  | Add of Id.t * var_or_imm | Sub of Id.t * var_or_imm
  | Shl of Id.t * int | Shr of Id.t * int
  | FNeg of Id.t | FAbs of Id.t
  | FInv of Id.t | Sqrt of Id.t
  | FAdd of fpu_sign * Id.t * Id.t
  | FSub of fpu_sign * Id.t * Id.t
  | FMul of fpu_sign * Id.t * Id.t
  | Eq of Id.t * Id.t | Ne of Id.t * Id.t
  | Lt of Id.t * Id.t | Le of Id.t * Id.t
  | FLt of Id.t * Id.t | FLe of Id.t * Id.t
  | IToF of Id.t | FToI of Id.t | Floor of Id.t
  | IfEq of Id.t * Id.t * t * t | IfNe of Id.t * Id.t * t * t
  | IfZ of Id.t * t * t | IfNz of Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Load of Id.t * int
  | Store of Id.t * Id.t * int
  | LoadL of Id.l * int
  | StoreL of Id.t * Id.l * int
  | ExtTuple of Id.l
  | ExtArray of Id.l

type fundef =
  { name:      Id.l * Type.t;
    args:      (Id.t * Type.t) list;
    formal_fv: (Id.t * Type.t) list;
    body:      t }

type prog = Prog of fundef list * t

let rec fv = function
  | Unit | Int _ | Float _  | LoadL _ | ExtTuple _ | ExtArray _ -> S.empty
  | Not x | Neg x | Add (x, C _) | Sub (x, C _) | Shl (x, _) | Shr (x, _)
  | FNeg x | FAbs x | FInv x | Sqrt x | IToF x | FToI x | Floor x
  | Load (x, _) | Var x | StoreL (x, _, _) -> S.singleton x
  | Add (x, V y) | Sub (x, V y) | FAdd (_, x, y) | FSub (_, x, y) | FMul (_, x, y)
  | Eq (x, y) | Ne (x, y) | Lt (x, y) | Le (x, y) | FLt (x, y) | FLe (x, y)
  | Store (x, y, _) -> S.of_list [x; y]
  | IfEq (x, y, e1, e2) | IfNe (x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | IfZ (x, e1, e2) | IfNz (x, e1, e2) -> S.add x (S.union (fv e1) (fv e2))
  | Let ((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | MakeCls ((x, t), { entry = l; actual_fv = ys }, e) -> S.remove x (S.union (S.of_list ys) (fv e))
  | AppCls (x, ys) -> S.of_list (x :: ys)
  | AppDir (_, xs) | Tuple xs -> S.of_list xs
  | LetTuple (xts, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xts)))

let toplevel : fundef list ref = ref []

let sign_conv = function
  | KNormal.Nop -> Nop
  | KNormal.Plus -> Plus
  | KNormal.Minus -> Minus
  | KNormal.Inv -> Inv

(* クロージャ変換ルーチン本体 *)
let rec g env known = function
  | KNormal.Unit -> Unit
  | KNormal.Int i -> Int i
  | KNormal.Float f -> Float f
  | KNormal.Not x -> Not x
  | KNormal.Neg x -> Neg x
  | KNormal.Add (x, KNormal.V y) -> Add (x, V y)
  | KNormal.Add (x, KNormal.C y) -> Add (x, C y)
  | KNormal.Sub (x, KNormal.V y) -> Sub (x, V y)
  | KNormal.Sub (x, KNormal.C y) -> Sub (x, C y)
  | KNormal.Shl (x, y) -> Shl (x, y)
  | KNormal.Shr (x, y) -> Shr (x, y)
  | KNormal.FNeg x -> FNeg x
  | KNormal.FAbs x -> FAbs x
  | KNormal.FInv x -> FInv x
  | KNormal.Sqrt x -> Sqrt x
  | KNormal.FAdd (s, x, y) -> FAdd (sign_conv s, x, y)
  | KNormal.FSub (s, x, y) -> FSub (sign_conv s, x, y)
  | KNormal.FMul (s, x, y) -> FMul (sign_conv s, x, y)
  | KNormal.Eq (x, y) -> Eq (x, y)
  | KNormal.Ne (x, y) -> Ne (x, y)
  | KNormal.Lt (x, y) -> Lt (x, y)
  | KNormal.Le (x, y) -> Le (x, y)
  | KNormal.FLt (x, y) -> FLt (x, y)
  | KNormal.FLe (x, y) -> FLe (x, y)
  | KNormal.IToF x -> IToF x
  | KNormal.FToI x -> FToI x
  | KNormal.Floor x -> Floor x
  | KNormal.IfEq (x, y, e1, e2) -> IfEq (x, y, g env known e1, g env known e2)
  | KNormal.IfNe (x, y, e1, e2) -> IfNe (x, y, g env known e1, g env known e2)
  | KNormal.IfZ (x, e1, e2) -> IfZ (x, g env known e1, g env known e2)
  | KNormal.IfNz (x, e1, e2) -> IfNz (x, g env known e1, g env known e2)
  | KNormal.Let ((x, t), e1, e2) -> Let ((x, t), g env known e1, g (M.add x t env) known e2)
  | KNormal.Var x -> Var x
  | KNormal.LetRec ({ KNormal.name = (x, t); KNormal.args = yts; KNormal.body = e1 }, e2) ->
      (*  関数定義 let rec x y1 ... yn = e1 in e2 の場合は、
          xに自由変数がない (closureを介さずdirectに呼び出せる) と仮定し、
          knownに追加してe1をクロージャ変換してみる  *)
      let toplevel_backup = !toplevel in
      let env' = M.add x t env in
      let known' = S.add x known in
      let e1' = g (M.add_list yts env') known' e1 in
      (* 本当に自由変数がなかったか、変換結果e1'を確認する *)
      (* 注意: e1'にx自身が変数として出現する場合はclosureが必要!
         (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml 参照) *)
      let zs = S.diff (fv e1') (S.of_list (List.map fst yts)) in
      let (known', e1') =
        if S.is_empty zs then (known', e1') else
        (* 駄目だったら状態(toplevelの値)を戻して、クロージャ変換をやり直す *)
        (if !Typing.lv >= 1 then Format.eprintf "[info] %s: free variable(s) found: %s@." x (Id.pp_list (S.elements zs));
         toplevel := toplevel_backup;
         let e1' = g (M.add_list yts env') known e1 in
         (known, e1')) in
      let zs = S.elements (S.diff (fv e1') (S.add x (S.of_list (List.map fst yts)))) in (* 自由変数のリスト *)
      let zts = List.map (fun z -> (z, M.find z env')) zs in (* ここで自由変数zの型を引くために引数envが必要 *)
      toplevel := { name = (Id.L x, t); args = yts; formal_fv = zts; body = e1' } :: !toplevel; (* トップレベル関数を追加 *)
      let e2' = g env' known' e2 in
      if S.mem x (fv e2') then (* xが変数としてe2'に出現するか *)
        MakeCls ((x, t), { entry = Id.L x; actual_fv = zs }, e2') (* 出現していたら削除しない *)
      else
        (if !Typing.lv >= 2 then Format.eprintf "[info] eliminating closure(s) %s@." x; e2') (* 出現しなければMakeClsを削除 *)
  | KNormal.App (x, ys) when S.mem x known ->
      if !Typing.lv >= 2 then Format.eprintf "[info] directly applying %s@." x;
      AppDir (Id.L x, ys)
  | KNormal.App (f, xs) -> AppCls (f, xs)
  | KNormal.Tuple xs -> Tuple xs
  | KNormal.LetTuple (xts, y, e) -> LetTuple (xts, y, g (M.add_list xts env) known e)
  | KNormal.Load (x, y) -> Load (x, y)
  | KNormal.Store (x, y, z) -> Store (x, y, z)
  | KNormal.LoadL (x, y) -> LoadL (Id.L x, y)
  | KNormal.StoreL (x, y, z) -> StoreL (x, Id.L y, z)
  | KNormal.ExtTuple x -> ExtTuple (Id.L x)
  | KNormal.ExtArray x -> ExtArray (Id.L x)
  | KNormal.ExtFunApp (x, ys) -> AppDir (Id.L x, ys)

let f e =
  toplevel := [];
  let e' = g M.empty S.empty e in
  Prog (List.rev !toplevel, e')

