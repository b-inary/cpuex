
(* 命令の列 *)
type t =
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t

(* 一つ一つの命令に対応する式 *)
and exp =
  | Nop
  | Li of int
  | Mov of Id.t
  | MovL of Id.l
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Addi of Id.t * int
  | Add4 of Id.t * Id.t * int
  | Sub of Id.t * Id.t
  | Shift of Id.t * int
  | Ld of Id.t * int
  | LdL of Id.l
  | St of Id.t * Id.t * int
  | FNeg of Id.t
  | FAbs of Id.t
  | FAdd of Id.t * Id.t
  | FMul of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | CallCls of Id.t * Id.t list
  | CallDir of Id.l * Id.t list
  | Save of Id.t * Id.t
  | Restore of Id.t

type fundef = { name: Id.l; args: Id.t list; body: t; ret: Type.t }

(* プログラム全体 = 浮動小数点数テーブル + トップレベル関数 + メインの式 *)
type prog = Prog of ((Id.l * int) list * (Id.l * float) list) * fundef list * t

let seq (e1, e2) = Let ((Id.gentmp Type.Unit, Type.Unit), e1, e2)

let regs = Array.init 12 (fun i -> Printf.sprintf "$%d" (i + 1))
let allregs = Array.to_list regs

let reg_sp = "$sp" (* stack pointer *)
let reg_bp = "$bp" (* base pointer *)
let reg_cl = regs.(Array.length regs - 1) (* closure address *)
let reg_sw = regs.(Array.length regs - 2) (* temporary for swap *)

let heap_addr = 0x4000

let is_reg x = (x.[0] = '$')

let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) *)
let rec fv_exp = function
  | Nop | Li _ | MovL _ | LdL _ | Restore _ -> []
  | Mov x | Neg x | Addi (x, _) | Shift (x, _) | Ld (x, _) | FNeg x | FAbs x | Save (x, _) -> [x]
  | Add (x, y) | Add4 (x, y, _) | Sub (x, y) | St (x, y, _) | FAdd (x, y) | FMul (x, y) -> [x; y]
  | IfEq (x, y, e1, e2) | IfLE (x, y, e1, e2) ->
      x :: y :: remove_and_uniq S.empty (fv e1 @ fv e2)
  | CallCls (x, ys) -> x :: ys
  | CallDir (_, ys) -> ys
and fv = function
  | Ans exp -> fv_exp exp
  | Let ((x, t), exp, e) -> fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)
let fv e = remove_and_uniq S.empty (fv e)

let rec concat e1 xt e2 =
  match e1 with
    | Ans exp -> Let (xt, exp, e2)
    | Let (yt, exp, e1') -> Let (yt, exp, concat e1' xt e2)

