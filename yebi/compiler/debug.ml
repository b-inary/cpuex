 
 open Asm

 let rec exp_to_str = function
  | Nop -> "(nop)"
  | Li d -> Printf.sprintf "(li %d)" d
  | Mov x -> Printf.sprintf "(mov %s)" x
  | MovL (Id.L x) -> Printf.sprintf "(movl %s)" x
  | Neg x -> Printf.sprintf "(neg %s)" x
  | Add (x, y) -> Printf.sprintf "(add %s, %s)" x y
  | Sub (x, y) -> Printf.sprintf "(sub %s, %s)" x y
  | Addi (x, d) -> Printf.sprintf "(addi %s, %d)" x d
  | Ld (x, d) -> Printf.sprintf "(ld [%s + %d])" x d
  | St (x, y, d) -> Printf.sprintf "(st %s, [%s + %d])" x y d
  | FNeg x -> Printf.sprintf "(fneg %s)" x
  | FAdd (x, y) -> Printf.sprintf "(fadd %s, %s)" x y
  | FMul (x, y) -> Printf.sprintf "(fmul %s, %s)" x y
  | IfEq (x, y, e, f) -> Printf.sprintf "(ifeq %s %s %s %s)" x y (t_to_str e) (t_to_str f)
  | IfLE (x, y, e, f) -> Printf.sprintf "(ifle %s %s %s %s)" x y (t_to_str e) (t_to_str f)
  | CallCls (x, y) -> Printf.sprintf "(callcls %s %s)" x (Id.pp_list y)
  | CallDir (Id.L x, y) -> Printf.sprintf "(calldir %s %s)" x (Id.pp_list y)
  | Push (x, y) -> Printf.sprintf "(push %s %s)" x y
  | Pop x -> Printf.sprintf "(pop %s)" x

and t_to_str = function
  | Ans e -> exp_to_str e
  | Let ((x, _), e, t) -> Printf.sprintf "(let %s %s %s)" x (exp_to_str e) (t_to_str t)

let rec fls_to_str = function
  | [] -> ""
  | (Id.L x, f) :: fls -> Printf.sprintf "%s = %f\n" x f ^ fls_to_str fls

let rec funs_to_str = function
  | [] -> ""
  | { name = Id.L x; args = a; body = t } :: funs ->
      Printf.sprintf "%s %s = %s\n" x (Id.pp_list a) (t_to_str t)

let prog_to_str (Prog (fls, funs, t)) =
  "[floating point constants]\n" ^ fls_to_str fls ^
  "[functions]\n" ^ funs_to_str funs ^
  "[main]\n" ^ t_to_str t

