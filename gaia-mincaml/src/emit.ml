
open Asm
open Printf

let counter = ref 0

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
    | y :: zs when x = y -> 0 :: List.map ((+) 4) (loc zs)
    | y :: zs -> List.map ((+) 4) (loc zs) in
  loc !stackmap

let offset x = List.hd (locate x) + 4
let stacksize () = List.length !stackmap * 4

(* 関数呼び出しのために引数を並べ替える *)
let rec shuffle sw xys =
  (* remove identical moves *)
  let (_, xys) = List.partition (fun (x, y) -> x = y) xys in
  (* find acyclic moves *)
  match List.partition (fun (_, y) -> List.mem_assoc y xys) xys with
    | ([], []) -> []
    | ((x, y) :: xys, []) -> (* no acyclic moves; resolve a cyclic move *)
        (y, sw) :: (x, y) ::
        shuffle sw (List.map (function (y', z) when y = y' -> (sw, z) | yz -> yz) xys)
    | (xys, acyc) -> acyc @ shuffle sw xys

(* 末尾かどうかを表すデータ型 *)
type dest = Tail | NonTail of Id.t

let lines = ref []
let al fmt = kprintf (fun s -> lines := (s ^ "\n") :: !lines) fmt

let rec print_rev oc = function
  | [] -> ()
  | l :: ls ->
      print_rev oc ls;
      if l = "pop stack\n" then begin
        let sz = stacksize () in
        fprintf oc "    leave\n";
        fprintf oc "    add     rsp, rsp, %d\n" (sz + 4)
      end else
        let l = String.map (fun c -> if c = '$' then 'r' else c) l in
        fprintf oc "%s" l

let addr x y =
  if x = "$0" then
    sprintf "[%d]" y
  else if y = 0 then
    sprintf "[%s]" x
  else if y > 0 then
    sprintf "[%s + %d]" x y
  else
    sprintf "[%s - %d]" x (-y)

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
  | (NonTail x, Li i)           -> al "    mov     %s, %d" x i
  | (NonTail x, Lf f)           -> al "    mov     %s, %F" x f
  | (NonTail x, Mov y) when x = y -> ()
  | (NonTail x, Mov y)          -> al "    mov     %s, %s" x y
  | (NonTail x, MovL (Id.L y))  -> al "    mov     %s, %s" x y
  | (NonTail x, Not y)          -> al "    cmpeq   %s, %s, 0" x y
  | (NonTail x, Neg y)          -> al "    neg     %s, %s" x y
  | (NonTail x, Add (y, V z))   -> al "    add     %s, %s, %s" x y z
  | (NonTail x, Add (y, C z)) when z = 0 -> g' oc (NonTail x, Mov y)
  | (NonTail x, Add (y, C z))   -> al "    add     %s, %s, %d" x y z
  | (NonTail x, Sub (y, V z))   -> al "    sub     %s, %s, %s" x y z
  | (NonTail x, Sub (y, C z)) when z = 0 -> g' oc (NonTail x, Mov y)
  | (NonTail x, Sub (y, C z))   -> al "    sub     %s, %s, %d" x y z
  | (NonTail x, Shl (y, z))     -> al "    shl     %s, %s, %d" x y z
  | (NonTail x, Shr (y, z))     -> al "    shr     %s, %s, %d" x y z
  | (NonTail x, FNeg y)         -> al "    fadd.neg %s, %s, $0" x y
  | (NonTail x, FAbs y)         -> al "    fadd.abs %s, %s, $0" x y
  | (NonTail x, FAdd (Nop, y, z))   -> al "    fadd    %s, %s, %s" x y z
  | (NonTail x, FAdd (Plus, y, z))  -> al "    fadd.abs %s, %s, %s" x y z
  | (NonTail x, FAdd (Minus, y, z)) -> al "    fadd.abs.neg %s, %s, %s" x y z
  | (NonTail x, FAdd (Inv, y, z))   -> al "    fadd.neg %s, %s, %s" x y z
  | (NonTail x, FSub (Nop, y, z))   -> al "    fsub    %s, %s, %s" x y z
  | (NonTail x, FSub (Plus, y, z))  -> al "    fsub.abs %s, %s, %s" x y z
  | (NonTail x, FSub (Minus, y, z)) -> al "    fsub.abs.neg %s, %s, %s" x y z
  | (NonTail x, FSub (Inv, y, z))   -> al "    fsub.neg %s, %s, %s" x y z
  | (NonTail x, FMul (Nop, y, z))   -> al "    fmul    %s, %s, %s" x y z
  | (NonTail x, FMul (Plus, y, z))  -> al "    fmul.abs %s, %s, %s" x y z
  | (NonTail x, FMul (Minus, y, z)) -> al "    fmul.abs.neg %s, %s, %s" x y z
  | (NonTail x, FMul (Inv, y, z))   -> al "    fmul.neg %s, %s, %s" x y z
  | (NonTail x, Eq (y, z))      -> al "    cmpeq   %s, %s, %s" x y z
  | (NonTail x, Ne (y, z))      -> al "    cmpne   %s, %s, %s" x y z
  | (NonTail x, Lt (y, z))      -> al "    cmplt   %s, %s, %s" x y z
  | (NonTail x, Le (y, z))      -> al "    cmple   %s, %s, %s" x y z
  | (NonTail x, FEq (y, z))     -> al "    fcmpeq  %s, %s, %s" x y z
  | (NonTail x, FNe (y, z))     -> al "    fcmpne  %s, %s, %s" x y z
  | (NonTail x, FLt (y, z))     -> al "    fcmplt  %s, %s, %s" x y z
  | (NonTail x, FLe (y, z))     -> al "    fcmple  %s, %s, %s" x y z
  | (NonTail x, IToF y)         -> al "    itof    %s, %s" x y
  | (NonTail x, FToI y)         -> al "    ftoi    %s, %s" x y
  | (NonTail x, Floor y)        -> al "    floor   %s, %s" x y
  | (NonTail x, Ld (y, z))          -> al "    mov     %s, %s" x (addr y z)
  | (NonTail x, LdL (Id.L y, z))    -> al "    mov     %s, %s" x (addr y z)
  | (NonTail _, St (x, y, z))       -> al "    mov     %s, %s" (addr y z) x
  | (NonTail _, StL (x, Id.L y, z)) -> al "    mov     %s, %s" (addr y z) x
  | (NonTail _, Save (x, y)) when List.mem x allregs && not (S.mem y !stackset) ->
      save y; al "    mov     %s, %s" (addr reg_bp (- (offset y))) x
  | (NonTail _, Save (x, y)) -> assert (S.mem y !stackset); ()
  | (NonTail x, Restore y) ->
      assert (List.mem x allregs);
      al "    mov     %s, %s" x (addr reg_bp (- (offset y)))
  (* 末尾だったら計算結果を第一レジスタにセットしてret *)
  | (Tail, (Nop | St _ | StL _ | Save _ as exp)) ->
      g' oc (NonTail (Id.gentmp Type.Unit), exp);
      al "    leave";
      al "    ret"
  | (Tail, (Li _ | Lf _ | Mov _ | MovL _ | Not _ | Neg _ | Add _ | Sub _ | Shl _ | Shr _ |
            FNeg _ | FAbs _ | FAdd _ | FSub _ | FMul _ | Eq _ | Ne _ | Lt _ | Le _ |
            FEq _ | FNe _ | FLt _ | FLe _ | IToF _ | FToI _ | Floor _ | Ld _ | LdL _ as exp)) ->
      g' oc (NonTail (regs.(0)), exp);
      al "    leave";
      al "    ret"
  | (Tail, (Restore x as exp)) ->
      (match locate x with
        | [i] -> g' oc (NonTail (regs.(0)), exp)
        | _ -> assert false);
      al "    leave";
      al "    ret"
  | (Tail, IfEq (x, y, e1, e2)) -> g'_tail_if oc x y e1 e2 "beq"
  | (Tail, IfNe (x, y, e1, e2)) -> g'_tail_if oc x y e1 e2 "bne"
  | (NonTail z, IfEq (x, y, e1, e2)) -> g'_non_tail_if oc (NonTail z) x y e1 e2 "beq"
  | (NonTail z, IfNe (x, y, e1, e2)) -> g'_non_tail_if oc (NonTail z) x y e1 e2 "bne"
  (* 関数呼び出しの仮想命令の実装 *)
  | (Tail, CallCls (x, ys)) ->
      g'_args oc [(x, reg_cl)] ys;
      al "    mov     %s, [%s]" reg_sw reg_cl;
      al "pop stack";
      al "    br      %s" reg_sw
  | (Tail, CallDir (Id.L x, ys)) ->
      g'_args oc [] ys;
      al "pop stack";
      al "    br      %s" x
  | (NonTail a, CallCls (x, ys)) ->
      g'_args oc [(x, reg_cl)] ys;
      al "    mov     %s, [%s]" reg_sw reg_cl;
      al "    call    %s" reg_sw;
      if List.mem a allregs && a <> regs.(0) then
        al "    mov     %s, %s" a regs.(0)
  | (NonTail a, CallDir (Id.L x, ys)) ->
      g'_args oc [] ys;
      al "    call    %s" x;
      if List.mem a allregs && a <> regs.(0) then
        al "    mov     %s, %s" a regs.(0)

and g'_tail_if oc x y e1 e2 b =
  let l = (incr counter; Printf.sprintf "L%d" !counter) in
  al "    %-3s     %s, %s, %s" b x y l;
  let stackset_back = !stackset in
  g oc (Tail, e2);
  al "%s:" l;
  stackset := stackset_back;
  g oc (Tail, e1)

and g'_non_tail_if oc dest x y e1 e2 b =
  let l1 = (incr counter; Printf.sprintf "L%d" !counter) in
  let l2 = (incr counter; Printf.sprintf "L%d" !counter) in
  al "    %-3s     %s, %s, %s" b x y l1;
  let stackset_back = !stackset in
  g oc (dest, e2);
  let stackset1 = !stackset in
  al "    br      %s" l2;
  al "%s:" l1;
  stackset := stackset_back;
  g oc (dest, e1);
  al "%s:" l2;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2

and g'_args oc x_reg_cl ys =
  let (i, yrs) = List.fold_left
                  (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
                  (0, x_reg_cl) ys in
  List.iter
    (fun (y, r) -> if y <> "$unit" then al "    mov     %s, %s" r y)
    (shuffle reg_sw yrs)

let h oc { name = Id.L x; args = _; body = e; ret = _ } =
  fprintf oc "%s:\n" x;
  stackset := S.empty;
  stackmap := [];
  lines := [];
  g oc (Tail, e);
  let sz = stacksize () in
  fprintf oc "    enter   %d\n" sz;
  print_rev oc !lines

let f oc (Prog (fundefs, e)) =
  List.iter (fun fundef -> h oc fundef) fundefs;
  fprintf oc ".global min_caml_main\nmin_caml_main:\n";
  stackset := S.empty;
  stackmap := [];
  lines := [];
  g oc (NonTail regs.(0), e);
  let sz = stacksize () in
  if sz > 0 then fprintf oc "    sub     rsp, rsp, %d\n" sz;
  print_rev oc !lines;
  fprintf oc "    halt\n"

