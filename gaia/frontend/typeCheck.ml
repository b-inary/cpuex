
open Mylib
open Type
open Ast
open Printf

let global_table = ref M.empty
let add_global =
  let id = ref (-1) in
  fun name ->
    incr id;
    let ret = sprintf "@%s.%d" name !id in
    global_table := M.add name ret !global_table;
    ret

let error expr msg =
  let expr_str =
    match expr with
        Let (name, e, _) -> ast_to_string (Let (name, e, Atom (Var "...")))
      | LetFun (name, args, e, _) -> ast_to_string (LetFun (name, args, e, Atom (Var "...")))
      | LetTpl (names, e, _) -> ast_to_string (LetTpl (names, e, Atom (Var "...")))
      | Seq (e, _) -> ast_to_string (Seq (e, Atom (Var "...")))
      | _ -> ast_to_string expr in
  eprintf "error: %s\nnote: following expression appeared\n %s\n" msg expr_str;
  exit 1

let unify_error expr expect actual =
  let exp = type_to_string expect in
  let act = type_to_string actual in
  error expr (sprintf "type error: expected %s, but got %s" exp act)

let unbound_error name =
  eprintf "error: unbound variable '%s'\n" name;
  exit 1


exception Unify of ty * ty (* expected type, actual type *)

let rec occur_check r = function
    TTuple ts -> List.iter (occur_check r) ts
  | TArray t -> occur_check r t
  | TFun (t, ts) -> occur_check r t; List.iter (occur_check r) ts
  | TVar r' when r == r' -> failwith ""
  | TVar {contents = Some t} -> occur_check r t
  | _ -> ()

let unify t1 t2 =
  let rec go t1 t2 =
    match t1, t2 with
        TUnit, TUnit | TBool, TBool | TInt, TInt | TFloat, TFloat -> ()
      | TTuple s1, TTuple s2 when List.length s1 = List.length s2 ->
          List.iter2 go s1 s2
      | TArray s1, TArray s2 -> go s1 s2
      | TFun (r1, s1), TFun (r2, s2) when List.length s1 = List.length s2 ->
          go r1 r2; List.iter2 go s1 s2
      | TVar r1, TVar r2 when r1 == r2 -> ()
      | TVar {contents = Some s1}, _ -> go s1 t2
      | _, TVar {contents = Some s2} -> go t1 s2
      | TVar ({contents = None} as r1), _ -> occur_check r1 t2; r1 := Some t2
      | _, TVar ({contents = None} as r2) -> occur_check r2 t1; r2 := Some t1
      | _ -> failwith "" in
  try go t1 t2 with Failure _ -> raise (Unify (t1, t2))

let rec infer env expr =
  try match expr with
      Atom Unit -> TUnit
    | Atom (Bool _) -> TBool
    | Atom (Int _) -> TInt
    | Atom (Float _) -> TFloat
    | Atom (Var name) when M.mem name env -> M.find name env
    | Atom (Var name) when M.mem name !global_table ->
        M.find (M.find name !global_table) env
    | Atom (Var name) -> unbound_error name
    | Not e -> unify TBool (infer env e); TBool
    | IOp (_, e1, e2) ->
        unify TInt (infer env e1);
        unify TInt (infer env e2);
        TInt
    | FOp (_, e1, e2) ->
        unify TFloat (infer env e1);
        unify TFloat (infer env e2);
        TFloat
    | FAbs e | FSqrt e -> unify TFloat (infer env e); TFloat
    | Cmp (_, e1, e2) -> unify (infer env e1) (infer env e2); TBool
    | App (Atom (Var func), args) ->
        let retty = new_tyvar () in
        let functy =
          if M.mem func env then M.find func env else
          if M.mem func !global_table then
            M.find (M.find func !global_table) env
          else unbound_error func in
        unify (TFun (retty, List.map (infer env) args)) functy;
        retty
    | App _ -> error expr "function must be an identifier"
    | Tuple es -> TTuple (List.map (infer env) es)
    | MakeAry (e1, e2) -> unify TInt (infer env e1); TArray (infer env e2)
    | Get (e1, e2) ->
        let t = new_tyvar () in
        unify (TArray t) (infer env e1);
        unify TInt (infer env e2);
        t
    | Put (e1, e2, e3) ->
        let t = infer env e3 in
        unify (TArray t) (infer env e1);
        unify TInt (infer env e2);
        TUnit
    | If (e1, e2, e3) ->
        unify TBool (infer env e1);
        let t = infer env e2 in
        unify t (infer env e3);
        t
    | Let (name, e1, e2) -> infer (M.add name (infer env e1) env) e2
    | LetFun _ -> error expr "function definition must be in top level"
    | LetTpl (names, e1, e2) ->
        let ts = List.map (fun _ -> new_tyvar ()) names in
        unify (TTuple ts) (infer env e1);
        infer (M.add_list names ts env) e2
    | Seq (e1, e2) -> unify TUnit (infer env e1); infer env e2
    | Dir (Read, e) -> unify TUnit (infer env e); TInt
    | Dir (Write, e) -> unify TInt (infer env e); TUnit
    | Dir (ItoF, e) -> unify TInt (infer env e); TFloat
    | Dir (FtoI, e) -> unify TFloat (infer env e); TInt
    | Dir (Floor, e) -> unify TFloat (infer env e); TFloat
    | Dir (CastInt, e) -> unify TFloat (infer env e); TInt
    | Dir (CastFloat, e) -> unify TInt (infer env e); TFloat
  with Unify (expect, actual) -> unify_error expr expect actual

let rec infer_global env expr =
  try match expr with
      Let (name, e1, e2) ->
        let ty = infer env e1 in
        let name' = add_global name in
        infer_global (M.add name' ty env) e2
    | LetFun (name, args, e1, e2) ->
        if List.length (List.sort_uniq compare args) < List.length args then
          error expr "duplicated argument name";
        let name' = add_global name in
        let argsty = List.map (fun a -> if a = "Unit" then TUnit else new_tyvar ()) args in
        let ty = TFun (new_tyvar (), argsty) in
        let env' = M.add name' ty env in
        let retty = infer (M.add_list args argsty env') e1 in
        unify (TFun (retty, argsty)) ty;
        infer_global env' e2
    | LetTpl (names, e1, e2) ->
        let ts = List.map (fun _ -> new_tyvar ()) names in
        unify (TTuple ts) (infer env e1);
        let names' = List.map add_global names in
        infer_global (M.add_list names' ts env) e2
    | Seq (e1, e2) ->
        unify TUnit (infer env e1);
        infer_global env e2
    | e -> ignore (infer env e); env
  with Unify (expect, actual) -> unify_error expr expect actual


let rec deref_tyvar = function
    TTuple ts -> TTuple (List.map deref_tyvar ts)
  | TArray t -> TArray (deref_tyvar t)
  | TFun (t, ts) -> TFun (deref_tyvar t, List.map deref_tyvar ts)
  | TVar {contents = None} -> TInt (* monomorphic *)
  | TVar {contents = Some t} -> deref_tyvar t
  | t -> t

let type_check expr =
  M.map deref_tyvar (infer_global M.empty expr)

