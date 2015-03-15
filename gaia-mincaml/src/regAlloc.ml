
open Asm

(* for register coalescing *)
(* Callがあったら、そこから先は無意味というか逆効果なので追わない。
   そのために「Callがあったかどうか」を返り値の第1要素に含める。 *)
let rec target' src (dest, t) = function
  | Mov x when x = src && is_reg dest ->
      assert (t <> Type.Unit);
      (false, [dest])
  | IfEq (_, _, e1, e2) | IfNe (_, _, e1, e2) ->
      let c1, rs1 = target src (dest, t) e1 in
      let c2, rs2 = target src (dest, t) e2 in
      (c1 && c2, rs1 @ rs2)
  | CallCls (x, ys) ->
      failwith "regalloc target callcls"
      (* (true, (target_args src regs 0 ys @ if x = src then [reg_cl] else [])) *)
  | CallDir (_, ys) ->
      (true, (target_args src regs 0 ys))
  | _ -> false, []

(* register targeting *)
and target src dest = function
  | Ans exp -> target' src dest exp
  | Let (xt, exp, e) ->
      let (c1, rs1) = target' src xt exp in
      if c1 then (true, rs1) else
      let (c2, rs2) = target src dest e in
      (c2, rs1 @ rs2)

(* auxiliary function for Call *)
and target_args src all n = function
  | [] -> []
  | y :: ys when src = y -> all.(n) :: target_args src all (n + 1) ys
  | _ :: ys -> target_args src all (n + 1) ys

(* allocにおいてspillingがあったかどうかを表すデータ型 *)
type alloc_result =
  | Alloc of Id.t (* allocated register *)
  | Spill of Id.t (* spilled variable *)

let rec alloc dest cont regenv x t =
  (* allocate a register or spill a variable *)
  assert (not (M.mem x regenv));
  let all =
    match t with
      | Type.Unit -> [] (* dummy *)
      | _ -> allregs in
  if all = [] then Alloc "$unit" else (* ad hoc *)
  if is_reg x then Alloc x else
  let free = fv cont in
  try
    let (c, prefer) = target x dest cont in
    (* 生きているレジスタ *)
    let live = List.fold_left
                (fun live y ->
                  if is_reg y then S.add y live else
                  try S.add (M.find y regenv) live
                  with Not_found -> live)
                S.empty free in
    (* そうでないレジスタを探す *)
    let r = List.find (fun r -> not (S.mem r live)) (prefer @ all) in
    (* Format.eprintf "allocated %s to %s@." x r; *)
    Alloc r
  with Not_found ->
    if !Typing.lv >= 2 then Format.eprintf "[info] register allocation failed for %s@." x;
    (* レジスタ変数を探す *)
    let y = List.find
              (fun y ->
                not (is_reg y) &&
                try List.mem (M.find y regenv) all
                with Not_found -> false)
              (List.rev free) in
    if !Typing.lv >= 3 then Format.eprintf "[info] spilling %s from %s@." y (M.find y regenv);
    Spill(y)

(* auxiliary function for g and g'_and_restore *)
let add x r regenv =
  if is_reg x then (assert (x = r); regenv) else
  M.add x r regenv

(* auxiliary functions for g' *)
exception NoReg of Id.t

let find x regenv =
  if is_reg x then x else
  try M.find x regenv
  with Not_found -> raise (NoReg x)

(* 命令列のレジスタ割り当て *)
let rec g dest cont regenv = function
  | Ans exp -> g'_and_restore dest cont regenv exp
  | Let ((x, t) as xt, exp, e) ->
      assert (not (M.mem x regenv));
      let cont' = concat e dest cont in
      let (e1', regenv1) = g'_and_restore xt cont' regenv exp in
      (match alloc dest cont' regenv1 x t with
        | Spill y ->
            let r = M.find y regenv1 in
            let (e2', regenv2) = g dest cont (add x r (M.remove y regenv1)) e in
            let save =
              try let r' = M.find y regenv in Save (r', y)
              with Not_found -> Nop in
            (seq (save, concat e1' (r, t) e2'), regenv2)
        | Alloc r ->
            let (e2', regenv2) = g dest cont (add x r regenv1) e in
            (concat e1' (r, t) e2', regenv2))

(* 使用される変数をスタックからレジスタへRestore *)
and g'_and_restore dest cont regenv exp =
  try g' dest cont regenv exp
  with NoReg x ->
    ((* Format.eprintf "restoring %s@." x; *)
     g dest cont regenv (Let ((x, Type.Int), Restore x, Ans exp)))

(* 各命令のレジスタ割り当て *)
and g' dest cont regenv = function
  | Nop | Li _ | Lf _ | MovL _ | LdL _ | Restore _ as exp -> (Ans exp, regenv)
  | Mov x -> (Ans (Mov (find x regenv)), regenv)
  | Not x -> (Ans (Not (find x regenv)), regenv)
  | Neg x -> (Ans (Neg (find x regenv)), regenv)
  | Add (x, V y) -> (Ans (Add (find x regenv, V (find y regenv))), regenv)
  | Add (x, C y) -> (Ans (Add (find x regenv, C y)), regenv)
  | Sub (x, V y) -> (Ans (Sub (find x regenv, V (find y regenv))), regenv)
  | Sub (x, C y) -> (Ans (Sub (find x regenv, C y)), regenv)
  | Shl (x, y) -> (Ans (Shl (find x regenv, y)), regenv)
  | Shr (x, y) -> (Ans (Shr (find x regenv, y)), regenv)
  | FNeg x -> (Ans (FNeg (find x regenv)), regenv)
  | FAbs x -> (Ans (FAbs (find x regenv)), regenv)
  | FInv x -> (Ans (FInv (find x regenv)), regenv)
  | Sqrt x -> (Ans (Sqrt (find x regenv)), regenv)
  | FAdd (s, x, y) -> (Ans (FAdd (s, find x regenv, find y regenv)), regenv)
  | FSub (s, x, y) -> (Ans (FSub (s, find x regenv, find y regenv)), regenv)
  | FMul (s, x, y) -> (Ans (FMul (s, find x regenv, find y regenv)), regenv)
  | Eq (x, y) -> (Ans (Eq (find x regenv, find y regenv)), regenv)
  | Ne (x, y) -> (Ans (Ne (find x regenv, find y regenv)), regenv)
  | Lt (x, y) -> (Ans (Lt (find x regenv, find y regenv)), regenv)
  | Le (x, y) -> (Ans (Le (find x regenv, find y regenv)), regenv)
  | FLt (x, y) -> (Ans (FLt (find x regenv, find y regenv)), regenv)
  | FLe (x, y) -> (Ans (FLe (find x regenv, find y regenv)), regenv)
  | IToF x -> (Ans (IToF (find x regenv)), regenv)
  | FToI x -> (Ans (FToI (find x regenv)), regenv)
  | Floor x -> (Ans (Floor (find x regenv)), regenv)
  | IfEq (x, y, e1, e2) as exp -> g'_if dest cont regenv exp (fun e1' e2' -> IfEq (find x regenv, find y regenv, e1', e2')) e1 e2
  | IfNe (x, y, e1, e2) as exp -> g'_if dest cont regenv exp (fun e1' e2' -> IfNe (find x regenv, find y regenv, e1', e2')) e1 e2
  | Ld (x, y) -> (Ans (Ld (find x regenv, y)), regenv)
  | St (x, y, z) -> (Ans (St (find x regenv, find y regenv, z)), regenv)
  | StL (x, y, z) -> (Ans (StL (find x regenv, y, z)), regenv)
  | CallCls (x, ys) as exp -> g'_call dest cont regenv exp (fun ys -> CallCls (find x regenv, ys)) ys
  | CallDir (l, ys) as exp -> g'_call dest cont regenv exp (fun ys -> CallDir (l, ys)) ys
  | Save (x, y) -> assert false

(* ifのレジスタ割り当て *)
and g'_if dest cont regenv exp constr e1 e2 =
  let (e1', regenv1) = g dest cont regenv e1 in
  let (e2', regenv2) = g dest cont regenv e2 in
  (* 両方に共通のレジスタ変数だけ利用 *)
  let regenv' =
    List.fold_left
      (fun regenv' x ->
        try
          if is_reg x then regenv' else
          let r1 = M.find x regenv1 in
          let r2 = M.find x regenv2 in
          if r1 <> r2 then regenv' else
          M.add x r1 regenv'
        with Not_found -> regenv')
      M.empty (fv cont) in
  (List.fold_left (fun e x ->
                    if x = fst dest || not (M.mem x regenv) || M.mem x regenv' then e else
                    seq (Save (M.find x regenv, x), e))
    (Ans (constr e1' e2')) (fv cont),
  regenv')

(* 関数呼び出しのレジスタ割り当て *)
and g'_call dest cont regenv exp constr ys =
  (List.fold_left
    (fun e x ->
      if x = fst dest || not (M.mem x regenv) then e else
      seq (Save (M.find x regenv, x), e))
    (Ans (constr (List.map (fun y -> find y regenv) ys))) (fv cont),
  M.empty)

(* 関数のレジスタ割り当て *)
let h { name = Id.L x; args = ys; body = e; ret = t } =
  let (i, arg_regs, regenv) =
    List.fold_left
      (fun (i, arg_regs, regenv) y ->
        let r = regs.(i) in
        (i + 1, arg_regs @ [r], (assert (not (is_reg y)); M.add y r regenv)))
      (0, [], M.empty) ys in
  let a =
    match t with
      | Type.Unit -> Id.gentmp Type.Unit
      | _ -> regs.(0) in
  let (e', regenv') = g (a, t) (Ans (Mov a)) regenv e in
  { name = Id.L x; args = arg_regs; body = e'; ret = t }

(* プログラム全体のレジスタ割り当て *)
let f (Prog (fundefs, e)) =
  let fundefs' = List.map h fundefs in
  let (e', regenv') = g (Id.gentmp Type.Unit, Type.Unit) (Ans Nop) M.empty e in
  Prog (fundefs', e')

