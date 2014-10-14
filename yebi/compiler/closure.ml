
type closure = { entry: Id.l; actual_fv: Id.t list }

(* クロージャ変換後の式 *)
type t =
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Addi of Id.t * int
  | Add4 of Id.t * Id.t * int
  | Sub of Id.t * Id.t
  | Shift of Id.t * int
  | FNeg of Id.t
  | FAbs of Id.t
  | FAdd of Id.t * Id.t
  | FMul of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtTuple of Id.l
  | ExtArray of Id.l

type fundef =
  { name:      Id.l * Type.t;
    args:      (Id.t * Type.t) list;
    formal_fv: (Id.t * Type.t) list;
    body:      t }

type prog = Prog of fundef list * t

let rec fv = function
  | Unit | Int _ | Float _  | ExtTuple _ | ExtArray _ -> S.empty
  | Neg x | Addi (x, _) | Shift (x, _) | FNeg x | FAbs x -> S.singleton x
  | Add (x, y) | Add4 (x, y, _) | Sub (x, y) | FAdd (x, y) | FMul (x, y) | Get (x, y) -> S.of_list [x; y]
  | IfEq (x, y, e1, e2)| IfLE (x, y, e1, e2) -> S.add x (S.add y (S.union (fv e1) (fv e2)))
  | Let ((x, t), e1, e2) -> S.union (fv e1) (S.remove x (fv e2))
  | Var x -> S.singleton x
  | MakeCls ((x, t), { entry = l; actual_fv = ys }, e) -> S.remove x (S.union (S.of_list ys) (fv e))
  | AppCls (x, ys) -> S.of_list (x :: ys)
  | AppDir (_, xs) | Tuple xs -> S.of_list xs
  | LetTuple (xts, y, e) -> S.add y (S.diff (fv e) (S.of_list (List.map fst xts)))
  | Put (x, y, z) -> S.of_list [x; y; z]

let toplevel : fundef list ref = ref []

(* クロージャ変換ルーチン本体 *)
let rec g env known = function
  | KNormal.Unit -> Unit
  | KNormal.Int   i -> Int i
  | KNormal.Float d -> Float d
  | KNormal.Add (x, y) -> Add (x, y)
  | KNormal.Addi (x, y) -> Addi (x, y)
  | KNormal.Add4 (x, y, z) -> Add4 (x, y, z)
  | KNormal.Sub (x, y) -> Sub (x, y)
  | KNormal.Shift (x, y) -> Shift(x, y)
  | KNormal.Neg  x -> Neg x
  | KNormal.FNeg x -> FNeg x
  | KNormal.FAbs x -> FAbs x
  | KNormal.FAdd (x, y) -> FAdd (x, y)
  | KNormal.FMul (x, y) -> FMul (x, y)
  | KNormal.IfEq (x, y, e1, e2) -> IfEq (x, y, g env known e1, g env known e2)
  | KNormal.IfLE (x, y, e1, e2) -> IfLE (x, y, g env known e1, g env known e2)
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
  | KNormal.Get (x, y) -> Get (x, y)
  | KNormal.Put (x, y, z) -> Put (x, y, z)
  | KNormal.ExtTuple x -> ExtTuple (Id.L x)
  | KNormal.ExtArray x -> ExtArray (Id.L x)
  | KNormal.ExtFunApp (x, ys) -> AppDir (Id.L x, ys)

let f e =
  toplevel := [];
  let e' = g M.empty S.empty e in
  Prog (List.rev !toplevel, e')

