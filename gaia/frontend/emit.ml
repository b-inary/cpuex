
open Mylib
open Type
open Ast
open Printf

let output_buf = ref []
let add_line fmt =
  ksprintf (fun s -> output_buf := sprintf "  %s" s :: !output_buf) fmt
let add_line_noindent fmt =
  ksprintf (fun s -> output_buf := s :: !output_buf) fmt

let id_counter = ref 0
let new_ident () =
  incr id_counter;
  "%" ^ string_of_int !id_counter

let current_label = ref "%0"
let label_table = ref M.empty
let new_label =
  let cnt = ref 0 in
  fun () ->
    incr cnt;
    "$" ^ string_of_int !cnt
let add_label l =
  let id = new_ident () in
  label_table := M.add l id !label_table;
  current_label := l;
  add_line_noindent "";
  add_line_noindent "; <label>:%s" (String.sub id 1 (String.length id - 1))

let global_buf = ref []


let atos = function
    Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Float f ->
      if sprintf "%F" f = sprintf "%.99F" f then
        sprintf "%F" f
      else
        let g = Int32.float_of_bits (Int32.bits_of_float f) in
        sprintf "0x%016Lx" (Int64.bits_of_float g)
  | Var v -> v
  | atom -> invalid_argf "atos: %s" (atom_to_string atom)

let rec ttos = function
    TBool -> "i1"
  | TInt -> "i32"
  | TFloat -> "float"
  | TTuple l -> "{" ^ String.concat ", " (List.map ttos l)  ^ "}*"
  | TArray t -> ttos t ^ "*"
  | TUnit -> "void"
  | ty -> invalid_argf "ttos: %s" (type_to_string ty)

let args_to_str args =
  let args' = List.filter (fun arg -> snd arg <> TUnit) args in
  String.concat ", " (List.map (fun (a, t) -> ttos t ^ " " ^ atos a) args')

let init_array ret a1 a2 t =
  let l0 = !current_label in
  let l1, l2 = new_label (), new_label () in
  let tmp = new_ident () in
  add_line "%s = icmp sgt i32 %s, 0" tmp (atos a1);
  add_line "br i1 %s, label %s, label %s" tmp l1 l2;
  add_label l1;
  let tmp2 = new_ident () in
  let tmp3 = new_ident () in
  let tmp4 = new_ident () in
  let tmp5 = new_ident () in
  add_line "%s = phi i32 [0, %s], [%s, %s]" tmp2 l0 tmp4 l1;
  add_line "%s = getelementptr %s* %s, i32 %s" tmp3 (ttos t) ret tmp2;
  add_line "store %s %s, %s* %s" (ttos t) (atos a2) (ttos t) tmp3;
  add_line "%s = add i32 %s, 1" tmp4 tmp2;
  add_line "%s = icmp slt i32 %s, %s" tmp5 tmp4 (atos a1);
  add_line "br i1 %s, label %s, label %s" tmp5 l1 l2;
  add_label l2

let rec var env name =
  let (atom, ty) = M.find name env in
  match atom with
      Var name when name.[0] = '@' ->
        begin match ty with
            TFun _ -> failwith "function is not a first-class object"
          | TArray t ->
              let sz = snd (List.assoc name !global_buf) in
              if sz = 0 then (Var "null", ty) else begin
                let ret = new_ident () in
                add_line "%s = bitcast [%d x %s]* %s to %s*" ret sz (ttos t) name (ttos t);
                (Var ret, ty)
              end
          | _ ->
              (Var name, ty)
        end
    | _ -> (atom, ty)

and unop env ty expr fmt =
  let atom = fst (insert_let env expr) in
  let ret = new_ident () in
  add_line "%s = %s" ret (sprintf fmt (atos atom));
  (Var ret, ty)

and binop env ty expr1 expr2 fmt =
  let atom1 = fst (insert_let env expr1) in
  let atom2 = fst (insert_let env expr2) in
  let ret = new_ident () in
  add_line "%s = %s" ret (sprintf fmt (atos atom1) (atos atom2));
  (Var ret, ty)

and cmp env expr1 expr2 icond fcond =
  let (atom1, ty) = insert_let env expr1 in
  let (atom2, _)  = insert_let env expr2 in
  let ret = new_ident () in
  if ty = TFloat then
    add_line "%s = fcmp %s float %s, %s" ret fcond (atos atom1) (atos atom2)
  else if ty = TBool || ty = TInt then
    add_line "%s = icmp %s %s %s, %s" ret icond (ttos ty) (atos atom1) (atos atom2)
  else
    failwith "invalid comparison";
  (Var ret, TBool)

and app env func args =
  let (func, ty) =
    match M.find func env with
        (Var func, TFun (ty, _)) -> (func, ty)
      | _ -> failwith "apply: something wrong" in
  let args' = List.map (insert_let env) args in
  if ty = TUnit then begin
    add_line "call %s %s(%s)" (ttos ty) func (args_to_str args');
    (Unit, ty)
  end else begin
    let ret = new_ident () in
    add_line "%s = call %s %s(%s)" ret (ttos ty) func (args_to_str args');
    (Var ret, ty)
  end

and tuple env exprs =
  let elems = List.filter (compose ((<>) TUnit) snd) (List.map (insert_let env) exprs) in
  let ty = TTuple (snd (List.split elems)) in
  let tmp = new_ident () in
  let ret = new_ident () in
  add_line "%s = call noalias i8* @malloc(i32 %d)" tmp (List.length exprs * 8);
  add_line "%s = bitcast i8* %s to %s" ret tmp (ttos ty);
  let store i (a, t) =
    let tmp = new_ident () in
    add_line "%s = getelementptr %s %s, i32 0, i32 %d" tmp (ttos ty) ret i;
    add_line "store %s %s, %s* %s" (ttos t) (atos a) (ttos t) tmp in
  List.iteri store elems;
  (Var ret, ty)

and create_array env expr1 expr2 =
  let atom1 = fst (insert_let env expr1) in
  let (atom2, t) = insert_let env expr2 in
  let ty = TArray t in
  let sz = match atom1 with
      Int i -> Int (i * 8)
    | Var v ->
        let ret = new_ident () in
        add_line "%s = shl i32 %s, 3" ret v;
        Var ret
    | _ -> failwith "create_array: something wrong" in
  if sz = Int 0 then (Var "null", ty) else begin
    let tmp = new_ident () in
    let ret = new_ident () in
    add_line "%s = call noalias i8* @malloc(i32 %s)" tmp (atos sz);
    add_line "%s = bitcast i8* %s to %s" ret tmp (ttos ty);
    init_array ret atom1 atom2 t;
    (Var ret, ty)
  end

and get env expr1 expr2 =
  let (atom1, ty) =
    match insert_let env expr1 with
        (atom, TArray ty) -> (atom, ty)
      | _ -> failwith "get: something wrong" in
  let atom2 = fst (insert_let env expr2) in
  let tmp = new_ident () in
  let ret = new_ident () in
  add_line "%s = getelementptr %s* %s, i32 %s" tmp (ttos ty) (atos atom1) (atos atom2);
  add_line "%s = load %s* %s" ret (ttos ty) tmp;
  (Var ret, ty)

and put env expr1 expr2 expr3 =
  let atom1 = fst (insert_let env expr1) in
  let atom2 = fst (insert_let env expr2) in
  let (atom3, ty) = insert_let env expr3 in
  let tmp = new_ident () in
  add_line "%s = getelementptr %s* %s, i32 %s" tmp (ttos ty) (atos atom1) (atos atom2);
  add_line "store %s %s, %s* %s" (ttos ty) (atos atom3) (ttos ty) tmp;
  (Unit, TUnit)

and branch env expr1 expr2 expr3 =
  let atom1 = fst (insert_let env expr1) in
  let l1, l2, l3 = new_label (), new_label (), new_label () in
  add_line "br i1 %s, label %s, label %s" (atos atom1) l1 l2;
  add_label l1;
  let (atom2, ty) = insert_let env expr2 in
  let l1' = !current_label in
  add_line "br label %s" l3;
  add_label l2;
  let atom3 = fst (insert_let env expr3) in
  let l2' = !current_label in
  add_line "br label %s" l3;
  add_label l3;
  if ty = TUnit then (Unit, TUnit) else begin
    let ret = new_ident () in
    add_line "%s = phi %s [%s, %s], [%s, %s]" ret (ttos ty) (atos atom2) l1' (atos atom3) l2';
    (Var ret, ty)
  end

and bind env name expr1 expr2 =
  let (atom, ty) = insert_let env expr1 in
  insert_let (M.add name (atom, ty) env) expr2

and bind_tuple env names expr1 expr2 =
  let (atom, ty) = insert_let env expr1 in
  let tys = match ty with
      TTuple t -> t
    | _ -> failwith "let_tuple: something wrong" in
  let rets = let_tuple atom ty tys in
  insert_let (M.add_list names rets env) expr2

and let_tuple atom ty tys =
  let load i t =
    let tmp = new_ident () in
    let ret = new_ident () in
    add_line "%s = getelementptr %s %s, i32 0, i32 %d" tmp (ttos ty) (atos atom) i;
    add_line "%s = load %s* %s" ret (ttos t) tmp;
    (Var ret, t) in
  List.mapi load tys

and dir atom = function
    Read ->
      let ret = new_ident () in
      add_line "%s = call i32 @read()" ret;
      (Var ret, TInt)
  | Write ->
      add_line "call void @write(i32 %s)" (atos atom);
      (Unit, TUnit)
  | ItoF ->
      let ret = new_ident () in
      add_line "%s = sitofp i32 %s to float" ret (atos atom);
      (Var ret, TFloat)
  | FtoI ->
      let ret = new_ident () in
      add_line "%s = call i32 @ftoi(float %s)" ret (atos atom);
      (Var ret, TInt)
  | Floor ->
      let ret = new_ident () in
      add_line "%s = call float @llvm.floor.f32(float %s)" ret (atos atom);
      (Var ret, TFloat)
  | CastInt ->
      let ret = new_ident () in
      add_line "%s = bitcast float %s to i32" ret (atos atom);
      (Var ret, TInt)
  | CastFloat ->
      let ret = new_ident () in
      add_line "%s = bitcast i32 %s to float" ret (atos atom);
      (Var ret, TFloat)

and insert_let env expr =
  try match expr with
      Atom Unit -> (Unit, TUnit)
    | Atom (Bool b) -> (Bool b, TBool)
    | Atom (Int i) -> (Int i, TInt)
    | Atom (Float f) -> (Float f, TFloat)
    | Atom (Var v) -> var env v
    | Not e -> unop env TBool e "xor i1 %s, 1"
    | IOp (Add, e1, e2) -> binop env TInt e1 e2 "add i32 %s, %s"
    | IOp (Sub, e1, e2) -> binop env TInt e1 e2 "sub i32 %s, %s"
    | IOp (Mul, e1, e2) -> binop env TInt e1 e2 "mul i32 %s, %s"
    | IOp (Div, e1, e2) -> binop env TInt e1 e2 "udiv i32 %s, %s"
    | IOp (Shl, e1, e2) -> binop env TInt e1 e2 "shl i32 %s, %s"
    | IOp (Shr, e1, e2) -> binop env TInt e1 e2 "lshr i32 %s, %s"
    | FOp (FAdd, e1, e2) -> binop env TFloat e1 e2 "fadd fast float %s, %s"
    | FOp (FSub, e1, e2) -> binop env TFloat e1 e2 "fsub fast float %s, %s"
    | FOp (FMul, e1, e2) -> binop env TFloat e1 e2 "fmul fast float %s, %s"
    | FOp (FDiv, e1, e2) -> binop env TFloat e1 e2 "fdiv fast float %s, %s"
    | FAbs e -> unop env TFloat e "call float @llvm.fabs.f32(float %s)"
    | FSqrt e -> unop env TFloat e "call float @llvm.sqrt.f32(float %s)"
    | Cmp (EQ, e1, e2) -> cmp env e1 e2 "eq" "oeq"
    | Cmp (NE, e1, e2) -> cmp env e1 e2 "ne" "one"
    | Cmp (LT, e1, e2) -> cmp env e1 e2 "slt" "olt"
    | Cmp (LE, e1, e2) -> cmp env e1 e2 "sle" "ole"
    | Cmp (GT, e1, e2) -> cmp env e1 e2 "sgt" "ogt"
    | Cmp (GE, e1, e2) -> cmp env e1 e2 "sge" "oge"
    | App (Atom (Var func), args) -> app env func args
    | App _ -> failwith "insert_let: app: something wrong"
    | Tuple l -> tuple env l
    | MakeAry (e1, e2) -> create_array env e1 e2
    | Get (e1, e2) -> get env e1 e2
    | Put (e1, e2, e3) -> put env e1 e2 e3
    | If (e1, e2, e3) -> branch env e1 e2 e3
    | Let (v, e1, e2) -> bind env v e1 e2
    | LetFun _ -> failwith "insert_let: letfun: something wrong"
    | LetTpl (l, e1, e2) -> bind_tuple env l e1 e2
    | Seq (e1, e2) -> ignore (insert_let env e1); insert_let env e2
    | Dir (d, e) -> dir (fst (insert_let env e)) d
  with Failure msg -> TypeCheck.error expr msg


let main_buf = ref []
let main_id = ref 0
let main_cur = ref "%0"
let global_id = ref (-1)
let reset () =
  output_buf := []; id_counter := 0; current_label := "%0"
let push_main () =
  main_buf := !output_buf; main_id := !id_counter; main_cur := !current_label
let pop_main () =
  output_buf := !main_buf; id_counter := !main_id; current_label := !main_cur

let add_global name ty sz =
  incr global_id;
  let name' = sprintf "@%s.%d" name !global_id in
  if ty <> TUnit then global_buf := (name', (ty, sz)) :: !global_buf;
  name'

let global_str (name, (ty, sz)) =
  match ty with
      TTuple l ->
        let s = ttos ty in
        let t = String.sub s 0 (String.length s - 1) in
        sprintf "%s = private global %s undef" name t
    | TArray t ->
        sprintf "%s = private global [%d x %s] undef" name sz (ttos t)
    | _ -> failwith "global_str: something wrong"

let add_func name =
  incr global_id;
  sprintf "@%s.%d" name !global_id

let fundef_str name args retty argsty =
  let f (a, t) = sprintf "%s %%%s" (ttos t) a in
  let args' = List.filter (fun arg -> snd arg <> TUnit) (List.combine args argsty) in
  let args_str = String.concat ", " (List.map f args') in
  sprintf "define private %s %s(%s) #0 {" (ttos retty) name args_str

let newline oc = output_string oc "\n"
let emit_string oc str = output_string oc str; newline oc

let emit_buf oc buf =
  let re = Str.regexp "\\$[0-9]+" in
  let subst s = M.find (Str.matched_string s) !label_table in
  let print line = emit_string oc (Str.global_substitute re subst line) in
  List.iter print (List.rev buf);
  newline oc

let emit oc globenv ast inlineall =
  let rec go env = function
      Let (name, Tuple l, e2) ->
        let elems = List.filter (compose ((<>) TUnit) snd) (List.map (insert_let env) l) in
        let ty = TTuple (snd (List.split elems)) in
        let name' = add_global name ty 0 in
        let store i (a, t) =
          let tmp = new_ident () in
          add_line "%s = getelementptr %s %s, i32 0, i32 %d" tmp (ttos ty) name' i;
          add_line "store %s %s, %s* %s" (ttos t) (atos a) (ttos t) tmp in
        List.iteri store elems;
        go (M.add name (Var name', ty) env) e2
    | Let (name, MakeAry (e1, e2), e3) ->
        let a1 = fst (insert_let env e1) in
        let (a2, t) = insert_let env e2 in
        let ty = TArray t in
        let sz = match a1 with
            Int i -> i
          | Var v -> failwith "create_array (global): size must be constant"
          | _ -> failwith "create_array (global): something wrong" in
        let name' = add_global name ty sz in
        if sz > 0 then begin
          let tmp = new_ident () in
          add_line "%s = bitcast [%d x %s]* %s to %s" tmp sz (ttos t) name' (ttos ty);
          init_array tmp a1 a2 t
        end;
        go (M.add name (Var name', ty) env) e3
    | Let (name, e1, e2) ->
        let (atom, ty) = insert_let env e1 in
        let name' = add_global name ty 0 in
        go (M.add name (Var name', ty) env) e2
    | LetFun (name, args, e1, e2) ->
        push_main ();
        reset ();
        let name' = add_func name in
        let ty = M.find name' globenv in
        let (retty, argsty) = match ty with
            TFun (r, a) -> (r, a)
          | _ -> failwith "emit: letfun: something wrong" in
        add_line_noindent "%s" (fundef_str name' args retty argsty);
        let env' = M.add name (Var name', ty) env in
        let argsmap = List.map2 (fun a t -> (Var ("%" ^ a), t)) args argsty in
        let extenv = M.add_list args argsmap env' in
        let atom = fst (insert_let extenv e1) in
        if retty = TUnit then add_line "ret void" else
        add_line "ret %s %s" (ttos retty) (atos atom);
        add_line_noindent "}";
        emit_buf oc !output_buf;
        pop_main ();
        go env' e2
    | Seq (e1, e2) ->
        ignore (insert_let env e1);
        go env e2
    | expr -> ignore (insert_let env expr) in
  newline oc;
  emit_string oc "target datalayout = \"p:32:32-i1:32-i8:32-n32\"";
  newline oc;
  add_line_noindent "define i32 @main() nounwind {";
  go M.empty ast;
  add_line "ret i32 0";
  add_line_noindent "}";
  emit_buf oc !output_buf;
  emit_buf oc (List.map global_str !global_buf);
  output_buf := [];
  add_line_noindent "declare i32 @read() nounwind";
  add_line_noindent "declare void @write(i32) nounwind";
  add_line_noindent "declare i32 @ftoi(float) nounwind readonly";
  add_line_noindent "declare float @llvm.fabs.f32(float) nounwind readonly";
  add_line_noindent "declare float @llvm.sqrt.f32(float) nounwind readonly";
  add_line_noindent "declare float @llvm.floor.f32(float) nounwind readonly";
  add_line_noindent "declare noalias i8* @malloc(i32) nounwind";
  add_line_noindent "";
  add_line_noindent "attributes #0 = { %s }"
    (if inlineall then "nounwind alwaysinline" else "nounwind");
  emit_buf oc !output_buf

