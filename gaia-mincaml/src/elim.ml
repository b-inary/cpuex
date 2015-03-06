
open KNormal

 (* 副作用の有無 *)
let rec effect = function
  | Let (_, e1, e2) | IfEq (_, _, e1, e2) | IfNe (_, _, e1, e2) | IfZ (_, e1, e2) | IfNz (_, e1, e2) ->
      effect e1 || effect e2
  | LetRec (_, e) | LetTuple (_, _, e) -> effect e
  | App _ | Store _ | StoreL _ | ExtFunApp _ -> true
  | _ -> false

 (* 副作用の有無 *)
let rec f = function
  | IfEq (x, y, e1, e2) -> IfEq (x, y, f e1, f e2)
  | IfNe (x, y, e1, e2) -> IfNe (x, y, f e1, f e2)
  | IfZ (x, e1, e2) -> IfZ (x, f e1, f e2)
  | IfNz (x, e1, e2) -> IfNz (x, f e1, f e2)
  | Let ((x, t), e1, e2) ->
      let e1' = f e1 in let e2' = f e2 in
      if effect e1' || S.mem x (fv e2') then Let((x, t), e1', e2') else
      (if !Typing.lv >= 2 then Format.eprintf "[info] eliminating variable %s@." x; e2')
  | LetRec ({ name = (x, t); args = yts; body = e1 }, e2) ->
      let e2' = f e2 in
      if S.mem x (fv e2') then
        LetRec({ name = (x, t); args = yts; body = f e1 }, e2')
      else
        (if !Typing.lv >= 2 then Format.eprintf "[info] eliminating function %s@." x; e2')
  | LetTuple (xts, y, e) ->
      let xs = List.map fst xts in
      let e' = f e in
      let live = fv e' in
      if List.exists (fun x -> S.mem x live) xs then LetTuple(xts, y, e') else
      (if !Typing.lv >= 2 then Format.eprintf "[info] eliminating variables %s@." (Id.pp_list xs); e')
  | e -> e

