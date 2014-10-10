
open Asm

(* すでにSaveされた変数の集合 *)
let stackset = ref S.empty

(* Saveされた変数の、スタックにおける位置 *)
let stackmap = ref []

let save x =
  stackset := S.add x !stackset;
  if not (List.mem x !stackmap) then
    stackmap := !stackmap @ [x]

let locate x =
  let rec loc = function
    | [] -> []
    | y :: zs when x = y -> 0 :: List.map succ (loc zs)
    | y :: zs -> List.map succ (loc zs) in
  loc !stackmap

let offset x = List.hd (locate x) + 1
let stacksize () = List.length !stackmap + 1

(* 関数呼び出しのために引数を並べ替える *)
let rec shuffle sw xys =
  (* remove identical moves *)
  let (_, xys) = List.partition (fun (x, y) -> x = y) xys in
  (* find acyclic moves *)
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
    | [], [] -> []
    | (x, y) :: xys, [] -> (* no acyclic moves; resolve a cyclic move *)
        (y, sw) :: (x, y) ::
        shuffle sw (List.map (function (y', z) when y = y' -> (sw, z) | yz -> yz) xys)
    | (xys, acyc) -> acyc @ shuffle sw xys

(* 末尾かどうかを表すデータ型 *)
type dest = Tail | NonTail of Id.t

let addr x y =
  if x = "$0" then
    Printf.sprintf "[%d]" y
  else if y = 0 then
    Printf.sprintf "[%s]" x
  else if y > 0 then
    Printf.sprintf "[%s + %d]" x y
  else
    Printf.sprintf "[%s - %d]" x (-y)

(* 命令列のアセンブリ生成 *)
let rec g oc = function
  | (dest, Ans exp) -> g' oc (dest, exp)
  | (dest, Let ((x, t), exp, e)) ->
      g' oc (NonTail x, exp);
      g oc (dest, e)

(* 各命令のアセンブリ生成 *)
and g' oc = function
  (* 末尾でなかったら計算結果をdestにセット *)
  | (NonTail _, Nop) -> ()
  | (NonTail x, Li i)           -> Printf.fprintf oc "    mov     %s, %d\n" x i
  | (NonTail x, MovL (Id.L y))  -> Printf.fprintf oc "    mov     %s, %s\n" x y
  | (NonTail x, Mov y) when x = y -> ()
  | (NonTail x, Mov y)          -> Printf.fprintf oc "    mov     %s, %s\n" x y
  | (NonTail x, Neg y)          -> Printf.fprintf oc "    neg     %s, %s\n" x y
  | (NonTail x, Add (y, z))     -> Printf.fprintf oc "    add     %s, %s, %s\n" x y z
  | (NonTail x, Addi (y, z)) when z = 0 -> g' oc (NonTail x, Mov y)
  | (NonTail x, Addi (y, z)) when z > 0 ->
                                   Printf.fprintf oc "    add     %s, %s, %d\n" x y z
  | (NonTail x, Addi (y, z))    -> Printf.fprintf oc "    sub     %s, %s, %d\n" x y (-z)
  | (NonTail x, Add4 (y, z, w)) -> Printf.fprintf oc "    add     %s, %s, %s, %d\n" x y z w
  | (NonTail x, Sub (y, z))     -> Printf.fprintf oc "    sub     %s, %s, %s\n" x y z
  | (NonTail x, Ld (y, z))      -> Printf.fprintf oc "    mov     %s, %s\n" x (addr y z)
  | (NonTail _, St (x, y, z))   -> Printf.fprintf oc "    mov     %s, %s\n" (addr y z) x
  | (NonTail x, FNeg y)         -> Printf.fprintf oc "    fneg    %s, %s\n" x y
  | (NonTail x, FAdd (y, z))    -> Printf.fprintf oc "    fadd    %s, %s, %s\n" x y z
  | (NonTail x, FMul (y, z))    -> Printf.fprintf oc "    fmul    %s, %s, %s\n" x y z
  | (NonTail _, Push (x, y)) when List.mem x allregs && not (S.mem y !stackset) ->
      save y; Printf.fprintf oc "    mov     %s, %s\n" (addr reg_bp (- (offset y))) x
  | (NonTail _, Push (x, y)) -> assert (S.mem y !stackset); ()
  | (NonTail x, Pop y) ->
      assert (List.mem x allregs);
      Printf.fprintf oc "    mov     %s, %s\n" x (addr reg_bp (- (offset y)))
  (* 末尾だったら計算結果を第一レジスタにセットしてret *)
  | (Tail, (Nop | St _ | Push _ as exp)) ->
      g' oc (NonTail (Id.gentmp Type.Unit), exp);
      Printf.fprintf oc "    ret\n";
  | (Tail, (Li _ | Mov _ | MovL _ | Neg _ | Add _ | Addi _ | Add4 _ | Sub _ | Ld _ | FNeg _ | FAdd _ | FMul _ as exp)) ->
      g' oc (NonTail (regs.(0)), exp);
      Printf.fprintf oc "    ret\n";
  | (Tail, (Pop x as exp)) ->
      (match locate x with
        | [i] -> g' oc (NonTail (regs.(0)), exp)
        | _ -> assert false);
      Printf.fprintf oc "    ret\n";
  | (Tail, IfEq (x, y, e1, e2)) -> g'_tail_if oc x y e1 e2 "bne"
  | (Tail, IfLE (x, y, e1, e2)) -> g'_tail_if oc x y e1 e2 "bgt"
  | (NonTail z, IfEq (x, y, e1, e2)) -> g'_non_tail_if oc (NonTail z) x y e1 e2 "bne"
  | (NonTail z, IfLE (x, y, e1, e2)) -> g'_non_tail_if oc (NonTail z) x y e1 e2 "bgt"
  (* 関数呼び出しの仮想命令の実装 *)
  | (Tail, CallCls (x, ys)) ->
      g'_args oc [(x, reg_cl)] ys;
      Printf.fprintf oc "    mov     %s, [%s]\n" reg_sw reg_cl;
      Printf.fprintf oc "    br      %s\n" reg_sw;
  | (Tail, CallDir (Id.L x, ys)) ->
      g'_args oc [] ys;
      Printf.fprintf oc "    br      %s\n" x;
  | (NonTail a, CallCls (x, ys)) ->
      g'_args oc [(x, reg_cl)] ys;
      Printf.fprintf oc "    mov     %s, [%s]\n" reg_sw reg_cl;
      Printf.fprintf oc "    call    %s\n" reg_sw;
      if List.mem a allregs && a <> regs.(0) then
        Printf.fprintf oc "    mov     %s, %s\n" a regs.(0)
  | (NonTail a, CallDir (Id.L x, ys)) ->
      g'_args oc [] ys;
      Printf.fprintf oc "    call    %s\n" x;
      if List.mem a allregs && a <> regs.(0) then
        Printf.fprintf oc "    mov     %s, %s\n" a regs.(0)

and g'_tail_if oc x y e1 e2 b =
  let b_else = Id.genid b in
  Printf.fprintf oc "    %-3s     %s, %s, %s\n" b x y b_else;
  let stackset_back = !stackset in
  g oc (Tail, e1);
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (Tail, e2)

and g'_non_tail_if oc dest x y e1 e2 b =
  let b_else = Id.genid b in
  let b_cont = Id.genid (b ^ "_cont") in
  Printf.fprintf oc "    %-3s     %s, %s, %s\n" b x y b_else;
  let stackset_back = !stackset in
  g oc (dest, e1);
  let stackset1 = !stackset in
  Printf.fprintf oc "    br      %s\n" b_cont;
  Printf.fprintf oc "%s:\n" b_else;
  stackset := stackset_back;
  g oc (dest, e2);
  Printf.fprintf oc "%s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2

and g'_args oc x_reg_cl ys =
  let (i, yrs) = List.fold_left
                  (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
                  (0, x_reg_cl) ys in
  List.iter
    (fun (y, r) -> Printf.fprintf oc "    mov     %s, %s\n" r y)
    (shuffle reg_sw yrs)

let h oc { name = Id.L x; args = _; body = e; ret = _ } =
  Printf.fprintf oc ".global %s\n" x;
  Printf.fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  g oc (Tail, e)

let f oc (Prog (data, fundefs, e)) =
  Printf.fprintf oc ".data\n";
  List.iter (fun (Id.L x, f) -> Printf.fprintf oc "%-11s .float  %.15g\n" (x ^ ":") f) (List.rev data);
  Printf.fprintf oc ".text\n";
  List.iter (fun fundef -> h oc fundef) fundefs;
  Printf.fprintf oc ".global main\nmain:\n";
  stackset := S.empty;
  stackmap := [];
  g oc (NonTail("$0"), e);

