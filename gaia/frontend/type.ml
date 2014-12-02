
open Printf

type ty =
    TUnit
  | TBool
  | TInt
  | TFloat
  | TTuple of ty list
  | TArray of ty
  | TFun of ty * ty list
  | TVar of ty option ref

let new_tyvar () = TVar (ref None)

let type_to_string t =
  let vars = ref [] in
  let rec go fmt = function
      TUnit -> "unit"
    | TBool -> "bool"
    | TInt -> "int"
    | TFloat -> "float"
    | TTuple ts -> sprintf "%a" go_list (ts, " * ", true)
    | TArray t -> sprintf "%a list" go t
    | TFun (rt, ats) -> sprintf "%a -> %a" go_list (ats, " -> ", false) go rt
    | TVar {contents = Some t} -> go fmt t
    | TVar ({contents = None} as r) ->
        if not (List.mem_assq r !vars) then
          vars := (r, Char.chr (97 + List.length !vars)) :: !vars;
        sprintf "'_%c" (List.assq r !vars)

  and go_list fmt = function
      [], _, _ -> ""
    | [TTuple _ as t], _, true | [TFun _ as t], _, true -> sprintf "(%a)" go t
    | [t], _, _ -> go fmt t
    | (TTuple _ as t)::ts, delim, (true as flag) | (TFun _ as t)::ts, delim, flag ->
        sprintf "(%a)%s%a" go t delim go_list (ts, delim, flag)
    | t::ts, delim, flag ->
        sprintf "%a%s%a" go t delim go_list (ts, delim, flag) in
  go () t

