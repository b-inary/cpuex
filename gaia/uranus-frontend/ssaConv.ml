
open Ast
open Type
open Printf


(* environment *)
module M = Map.Make(String)
type env = (ast * ty) M.t

(* output *)
let add_line s = printf "  %s\n" s
let add_label l = printf "%s:\n" l

(* unnamed identifiers *)
let id_counter = ref (-1)
let label_counter = ref (-1)
let new_ident () =
  incr id_counter;
  "%" ^ string_of_int !id_counter
let new_label () =
  incr label_counter;
  "L" ^ string_of_int !label_counter


(* ast -> string *)
let atom_to_str = function
    Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Float f ->
      if sprintf "%F" f = sprintf "%.99F" f then
        sprintf "%F" f
      else
        let g = Int32.float_of_bits (Int32.bits_of_float f) in
        sprintf "0x%016Lx" (Int64.bits_of_float g)
  | Var v -> v
  | ast -> failwith ("atom_to_str: " ^ string_of_ast ast)

(* ty -> string *)
let rec type_to_str = function
    Tbool -> "i1"
  | Tint -> "i32"
  | Tfloat -> "float"
  | Ttuple l -> "{" ^ String.concat ", " (List.map type_to_str l)  ^ "}*"
  | Tarray t -> type_to_str t ^ "*"
  | _ -> failwith "type_to_str"

(* (ast * ty) list -> string *)
let args_to_str args =
  String.concat ", " (List.map (fun (a, t) -> type_to_str t ^ " " ^ atom_to_str a) args)


(* ty -> env -> ast -> format -> (ast * ty) *)
let rec unop ty env expr fmt =
  let atom = fst (insert_let env expr) in
  let ret = new_ident () in
  add_line (ret ^ " = " ^ sprintf fmt (atom_to_str atom));
  (Var ret, ty)

(* ty -> env -> ast -> ast -> format -> (ast * ty) *)
and binop ty env expr1 expr2 fmt =
  let atom1 = fst (insert_let env expr1) in
  let atom2 = fst (insert_let env expr2) in
  let ret = new_ident () in
  add_line (ret ^ " = " ^ sprintf fmt (atom_to_str atom1) (atom_to_str atom2));
  (Var ret, ty)

(* env -> ast -> ast -> string -> string -> (ast * ty) *)
and cmp env expr1 expr2 icond fcond =
  let (atom1, ty) = insert_let env expr1 in
  let (atom2, _)  = insert_let env expr2 in
  let ret = new_ident () in
  if ty = Tfloat then
    add_line (sprintf "%s = fcmp %s float %s, %s" ret fcond (atom_to_str atom1) (atom_to_str atom2))
  else
    add_line (sprintf "%s = icmp %s %s %s, %s" ret icond (type_to_str ty) (atom_to_str atom1) (atom_to_str atom2));
  (Var ret, Tbool)

(* env -> string -> ast list -> (ast * ty) *)
and app env func args =
  let (func, ty) =
    match M.find func env with
        (Var func, Tfun (ty, _)) -> (func, ty)
      | _ -> failwith ("app") in
  let atoms = List.map (insert_let env) args in
  if ty = Tunit then begin
    add_line (sprintf "call %s %s(%s)" (type_to_str ty) func (args_to_str atoms));
    (Unit, ty)
  end else begin
    let ret = new_ident () in
    add_line (sprintf "%s = call %s %s(%s)" ret (type_to_str ty) func (args_to_str atoms));
    (Var ret, ty)
  end

(* env -> ast -> ast -> (ast * ty) *)
and get env expr1 expr2 =
  let (atom1, ty) =
    match insert_let env expr1 with
        (atom, Tarray ty) -> (atom, ty)
      | _ -> failwith "get" in
  let atom2 = fst (insert_let env expr2) in
  let tmp = new_ident () in
  let ret = new_ident () in
  add_line (sprintf "%s = getelementptr %s* %s, i32 %s" tmp (type_to_str ty) (atom_to_str atom1) (atom_to_str atom2));
  add_line (sprintf "%s = load %s* %s" ret (type_to_str ty) tmp);
  (Var ret, ty)

(* env -> ast -> ast -> ast -> (ast * ty) *)
and put env expr1 expr2 expr3 =
  let atom1 = fst (insert_let env expr1) in
  let atom2 = fst (insert_let env expr2) in
  let (atom3, ty) = insert_let env expr3 in
  let tmp = new_ident () in
  add_line (sprintf "%s = getelementptr %s* %s, i32 %s" tmp (type_to_str ty) (atom_to_str atom1) (atom_to_str atom2));
  add_line (sprintf "store %s %s, %s* %s" (type_to_str ty) (atom_to_str atom3) (type_to_str ty) tmp);
  (Unit, Tunit)

(* env -> ast -> ast -> ast -> (ast * ty) *)
and branch env expr1 expr2 expr3 =
  let atom1 = fst (insert_let env expr1) in
  let l1 = new_label () in
  let l2 = new_label () in
  let l3 = new_label () in
  add_line (sprintf "br i1 %s, label %%%s, label %%%s" (atom_to_str atom1) l1 l2);
  add_label l1;
  let (atom2, ty) = insert_let env expr2 in
  add_line (sprintf "br label %%%s" l3);
  add_label l2;
  let atom3 = fst (insert_let env expr3) in
  add_line (sprintf "br label %%%s" l3);
  add_label l3;
  let ret = new_ident () in
  add_line (sprintf "%s = phi %s [%s, %%%s], [%s, %%%s]" ret (type_to_str ty) (atom_to_str atom2) l1 (atom_to_str atom3) l2);
  (Var ret, ty)

and bind env var expr1 expr2 =
  let (atom, ty) = insert_let env expr1 in
  insert_let (M.add var (atom, ty) env) expr2

(* (ast * ty) M.t -> ast -> (ast * ty) *)
and insert_let env = function
    Unit -> (Unit, Tunit)
  | Bool b -> (Bool b, Tbool)
  | Int i -> (Int i, Tint)
  | Float f -> (Float f, Tfloat)
  | Var v -> M.find v env
  | Not e -> unop Tint env e "xor i1 %s, 1"
  | Iop (Add, e1, e2) -> binop Tint env e1 e2 "add i32 %s, %s"
  | Iop (Sub, e1, e2) -> binop Tint env e1 e2 "sub i32 %s, %s"
  | Iop (Mul, e1, e2) -> binop Tint env e1 e2 "mul i32 %s, %s"
  | Iop (Div, e1, e2) -> binop Tint env e1 e2 "udiv i32 %s, %s"
  | Fop (Fadd, e1, e2) -> binop Tfloat env e1 e2 "fadd fast float %s, %s"
  | Fop (Fsub, e1, e2) -> binop Tfloat env e1 e2 "fsub fast float %s, %s"
  | Fop (Fmul, e1, e2) -> binop Tfloat env e1 e2 "fmul fast float %s, %s"
  | Fop (Fdiv, e1, e2) -> binop Tfloat env e1 e2 "fdiv fast float %s, %s"
  | Fabs e -> unop Tfloat env e "call float @llvm.fabs.f32(float %s)"
  | Cmp (EQ, e1, e2) -> cmp env e1 e2 "eq" "oeq"
  | Cmp (NE, e1, e2) -> cmp env e1 e2 "ne" "one"
  | Cmp (LT, e1, e2) -> cmp env e1 e2 "slt" "olt"
  | Cmp (LE, e1, e2) -> cmp env e1 e2 "sle" "ole"
  | Cmp (GT, e1, e2) -> cmp env e1 e2 "sgt" "ogt"
  | Cmp (GE, e1, e2) -> cmp env e1 e2 "sge" "oge"
  | App (Var func, args) -> app env func args
  | App _ -> failwith "insert_let (app)"
(*
  | Tuple l ->
  | MakeAry (e1, e2) ->
*)
  | Get (e1, e2) -> get env e1 e2
  | Put (e1, e2, e3) -> put env e1 e2 e3
  | If (e1, e2, e3) -> branch env e1 e2 e3
  | Let (v, e1, e2) -> bind env v e1 e2
(*
  | LetTpl (l, e1, e2) ->
*)
  | Seq (e1, e2) -> ignore (insert_let env e1); insert_let env e2
  | _ -> failwith "insert_let"


(* ast -> unit *)
let ssa_conv ast =
  let (atom, ty) = insert_let M.empty ast in
  add_line (sprintf "ret %s %s" (type_to_str ty) (atom_to_str atom))

