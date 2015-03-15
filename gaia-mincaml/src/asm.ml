
type val_or_imm = V of Id.t | C of int
type fpu_sign = Nop | Plus | Minus | Inv

(* 命令の列 *)
type t =
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t

(* 一つ一つの命令に対応する式 *)
and exp =
  | Nop
  | Li of int
  | Lf of float
  | Mov of Id.t
  | MovL of Id.l
  | Not of Id.t
  | Neg of Id.t
  | Add of Id.t * val_or_imm
  | Sub of Id.t * val_or_imm
  | Shl of Id.t * int
  | Shr of Id.t * int
  | FNeg of Id.t
  | FAbs of Id.t
  | FInv of Id.t
  | Sqrt of Id.t
  | FAdd of fpu_sign * Id.t * Id.t
  | FSub of fpu_sign * Id.t * Id.t
  | FMul of fpu_sign * Id.t * Id.t
  | Eq of Id.t * Id.t
  | Ne of Id.t * Id.t
  | Lt of Id.t * Id.t
  | Le of Id.t * Id.t
  | FLt of Id.t * Id.t
  | FLe of Id.t * Id.t
  | IToF of Id.t
  | FToI of Id.t
  | Floor of Id.t
  | IfEq of Id.t * Id.t * t * t
  | IfNe of Id.t * Id.t * t * t
  | Ld of Id.t * int
  | LdL of Id.l * int
  | St of Id.t * Id.t * int
  | StL of Id.t * Id.l * int
  | CallCls of Id.t * Id.t list
  | CallDir of Id.l * Id.t list
  | Save of Id.t * Id.t
  | Restore of Id.t

type fundef = { name: Id.l; args: Id.t list; body: t; ret: Type.t }

(* プログラム全体 = トップレベル関数 + メインの式 *)
type prog = Prog of fundef list * t

let seq (e1, e2) = Let ((Id.gentmp Type.Unit, Type.Unit), e1, e2)

let regs = Array.init 27 (fun i -> Printf.sprintf "$%d" (i + 1))
let allregs = Array.to_list regs

let reg_sp = "rsp" (* stack pointer *)
(* let reg_bp = "$bp" (* base pointer *) *)
(* let reg_cl = regs.(Array.length regs - 1) (* closure address *) *)
let reg_sw = regs.(Array.length regs - 1) (* temporary for swap *)

let is_reg x = (x.[0] = '$')

let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys

(* free variables in the order of use (for spilling) *)
let rec fv_exp = function
  | Nop | Li _ | Lf _ | MovL _ | LdL _ | Restore _ -> []
  | Mov x | Not x | Neg x | Add (x, C _) | Sub (x, C _) | Shl (x, _) | Shr (x, _)
  | FNeg x | FAbs x | FInv x | Sqrt x | IToF x | FToI x | Floor x
  | Ld (x, _) | StL (x, _, _) | Save (x, _) -> [x]
  | Add (x, V y) | Sub (x, V y) | FAdd (_, x, y) | FSub (_, x, y) | FMul (_, x, y)
  | Eq (x, y) | Ne (x, y) | Lt (x, y) | Le (x, y) | FLt (x, y) | FLe (x, y) | St (x, y, _) -> [x; y]
  | IfEq (x, y, e1, e2) | IfNe (x, y, e1, e2) -> x :: y :: remove_and_uniq S.empty (fv e1 @ fv e2)
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

