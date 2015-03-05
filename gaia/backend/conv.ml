
open Util
open Asm

let tmpsym =
let counter = ref (-1) in
fun () ->
incr counter;
Printf.sprintf "!tmp%d" !counter

let alu4op name op v1 v2 =
  let v1, v2, imm = match v1, v2 with
    | Llvm.Var v1, Llvm.Var v2 -> v1, v2, 0
    | Llvm.Var v, Llvm.Int i | Llvm.Int i, Llvm.Var v -> v, "#r0", i
    | _ -> failwithf "alu4op" in
  let op = match op with
    | Llvm.Add -> Add | Llvm.Sub -> Sub
    | Llvm.Shl -> Shl | Llvm.Shr -> Shr
    | Llvm.And -> And | Llvm.Or -> Or | Llvm.Xor -> Xor
    | Llvm.EQ -> CmpEQ | Llvm.NE -> CmpNE
    | Llvm.LT -> CmpLT | Llvm.LE -> CmpLE
    | Llvm.GT -> CmpGT | Llvm.GE -> CmpGE
    | _ -> failwithf "alu4op: op" in
  [Def (name, Alu4 (op, v1, v2, imm))]

let alu3op name op v1 v2 =
  let pre, v1, v2 = match v1, v2 with
    | Llvm.Var v1, Llvm.Var v2 -> [], v1, v2
    | Llvm.Var v, Llvm.Float f | Llvm.Float f, Llvm.Var v ->
      let sym = tmpsym () in [Def (sym, MovF f)], v, sym
    | _ -> failwithf "alu3op" in
  let op = match op with
    | Llvm.FEQ -> FCmpEQ | Llvm.FNE -> FCmpNE
    | Llvm.FLT -> FCmpLT | Llvm.FLE -> FCmpLE
    | Llvm.FGT -> FCmpGT | Llvm.FGE -> FCmpGE
    | _ -> failwithf "alu3op: op" in
  pre @ [Def (name, Alu3 (op, v1, v2))]

let fpuop name op v1 v2 =
  let pre, v1, v2 = match v1, v2 with
    | Llvm.Var v1, Llvm.Var v2 -> [], v1, v2
    | Llvm.Var v, Llvm.Float f | Llvm.Float f, Llvm.Var v ->
      let sym = tmpsym () in [Def (sym, MovF f)], v, sym
    | _ -> failwithf "fpuop" in
  let op = match op with
    | Llvm.FAdd -> FAdd | Llvm.FSub -> FSub
    | Llvm.FMul -> FMul | Llvm.FDiv -> FDiv
    | _ -> failwithf "fpuop: op" in
  pre @ [Def (name, Fpu3 (op, Nop, v1, v2))]

let conv_def name = function
  | Llvm.Mov v -> [Def (name, Mov v)]
  | Llvm.Bin (op, v1, v2) ->
    begin match op with
    | Llvm.Add | Llvm.Sub | Llvm.Shl | Llvm.Shr
    | Llvm.And | Llvm.Or | Llvm.Xor
    | Llvm.EQ | Llvm.NE | Llvm.LT | Llvm.LE | Llvm.GT | Llvm.GE ->
      alu4op name op v1 v2
    | Llvm.FEQ | Llvm.FNE | Llvm.FLT | Llvm.FLE | Llvm.FGT | Llvm.FGE ->
      alu3op name op v1 v2
    | Llvm.FAdd | Llvm.FSub | Llvm.FMul | Llvm.FDiv ->
      fpuop name op v1 v2
    end
  | Llvm.IToF v ->
    let pre, v = match v with
      | Llvm.Var v -> [], v
      | Llvm.Int i -> let sym = tmpsym () in [Def (sym, MovI i)], sym
      | _ -> failwithf "conv_def: itof" in
    pre @ [Def (name, Fpu2 (IToF, Nop, v))]
  | Llvm.Load (v1, v2) ->
    let v = match v1 with
      | Llvm.Var v -> v
      | Llvm.Global v -> "@" ^ v
      | _ -> failwithf "conv_def: load: v1" in
    let pre, i = match v2 with
      | Llvm.Var v -> let sym = tmpsym () in [Def (sym, Alu4 (Shl, v, "#r0", 2))], 0
      | Llvm.Int i -> [], 4 * i
      | _ -> failwithf "conv_def: load: v2" in
    pre @ [Def (name, Load (v, i))]
  | Llvm.Gep (v1, v2) ->
    let v1 = match v1 with
      | Llvm.Var v -> v
      | Llvm.Global v -> "@" ^ v
      | _ -> failwithf "conv_def: gep: v1" in
    begin match v2 with
    | Llvm.Var v ->
      let sym = tmpsym () in
      [Def (sym, Alu4 (Shl, v, "", 2)); Def (name, Alu4 (Add, v1, sym, 0))]
    | Llvm.Int i -> [Def (name, Alu4 (Add, v1, "#r0", 4 * i))]
    | _ -> failwithf "conv_def: gep: v2"
    end
  | Llvm.Select (s, v1, v2) ->
    let sym = tmpsym () in
    let pre = match v1, v2 with
      | Llvm.Int i1, Llvm.Int i2 ->
        [Def (sym, MovI (i1 lxor i2))]
      | Llvm.Float f1, Llvm.Float f2 ->
        let f = Int32.logxor (Int32.bits_of_float f1) (Int32.bits_of_float f2) in
        [Def (sym, MovI (Int32.to_int f))]
      | Llvm.Var v, Llvm.Int i | Llvm.Int i, Llvm.Var v ->
        [Def (sym, Alu4 (Xor, v, "#r0", i))]
      | Llvm.Var v, Llvm.Float f | Llvm.Float f, Llvm.Var v ->
        let sym2 = tmpsym () in
        [Def (sym2, MovF f); Def (sym, Alu4 (Xor, v, sym2, 0))]
      | Llvm.Var v1, Llvm.Var v2 ->
        [Def (sym, Alu4 (Xor, v1, v2, 0))]
      | _ -> failwithf "conv_def: select" in
    let flg = tmpsym () in
    let res = match v1 with
      | Llvm.Int i -> [Def (name, Alu4 (Xor, flg, "#r0", i))]
      | Llvm.Float f -> [Def (name, Alu4 (Xor, flg, "#r0", Int32.to_int (Int32.bits_of_float f)))]
      | Llvm.Var v -> [Def (name, Alu4 (Xor, flg, v, 0))]
      | _ -> failwithf "conv_def: select (res)" in
    pre @ [Def (flg, Alu4 (And, sym, s, -1))] @ res
  | Llvm.Call ("read", []) -> [Def (name, Read)]
  | Llvm.Call ("ftoi", v) ->
    begin match v with
    | [Llvm.Var v] -> [Def (name, Fpu2 (FToI, Nop, v))]
    | _ -> failwithf "conv_def: ftoi"
    end
  | Llvm.Call ("llvm.fabs.f32", v) ->
    begin match v with
    | [Llvm.Var v] -> [Def (name, Fpu3 (FAdd, Abs, v, "#r0"))]
    | _ -> failwithf "conv_def: fabs"
    end
  | Llvm.Call ("llvm.sqrt.f32", v) ->
    begin match v with
    | [Llvm.Var v] -> [Def (name, Fpu2 (FSqrt, Nop, v))]
    | _ -> failwith "conv_def: sqrt"
    end
  | Llvm.Call ("llvm.floor.f32", v) ->
    begin match v with
    | [Llvm.Var v] -> [Def (name, Fpu2 (Floor, Nop, v))]
    | _ -> failwith "conv_def: floor"
    end
  | Llvm.Call ("malloc", v) ->
    let sym = tmpsym () in
    let a = match v with
      | [Llvm.Int i] -> [Def (sym, Alu4 (Add, "@heap_ptr", "#r0", i))]
      | [Llvm.Var v] -> [Def (sym, Alu4 (Add, "@heap_ptr", v, 0))]
      | _ -> failwith "conv_def: malloc" in
    [Def (name, Load ("@heap_ptr", 0))] @ a @ [Store (sym, "@heap_ptr", 0)]
  | Llvm.Call (f, args) ->
    let go v (pre, args) =
      match v with
      | Llvm.Int i -> let sym = tmpsym () in (Def (sym, MovI i) :: pre, sym :: args)
      | Llvm.Float f -> let sym = tmpsym () in (Def (sym, MovF f) :: pre, sym :: args)
      | Llvm.Var v -> (pre, v :: args)
      | _ -> failwith "conv_def: call" in
    let pre, args = List.fold_right go args ([], []) in
    pre @ [Def (name, Call (f, args))]
  | Llvm.Phi vss ->
    let go v (pre, vs) =
      match v with
      | Llvm.Int i -> let sym = tmpsym () in (Def (sym, MovI i) :: pre, sym :: vs)
      | Llvm.Float f -> let sym = tmpsym () in (Def (sym, MovF f) :: pre, sym :: vs)
      | Llvm.Var v -> (pre, v :: vs)
      | _ -> failwith "conv_def: phi" in
    let pre, vs = List.fold_right go (List.map fst vss) ([], []) in
    pre @ [Def (name, Phi (List.map2 (fun x y -> (x, snd y)) vs vss))]

let conv_inst = function
  | Llvm.Def (name, def) -> conv_def name def
  | Llvm.Store (v, base, ofs) ->
    let pre, v = match v with
      | Llvm.Int i -> let sym = tmpsym () in [Def (sym, MovI i)], sym
      | Llvm.Float f -> let sym = tmpsym () in [Def (sym, MovF f)], sym
      | Llvm.Var v -> [], v
      | _ -> failwithf "conv_inst: store (v)" in
    let base = match base with
      | Llvm.Var v -> v
      | Llvm.Global v -> "@" ^ v
      | _ -> failwithf "conv_inst: store (base)" in
    begin match ofs with
    | Llvm.Int i -> pre @ [Store (v, base, i)]
    | Llvm.Var v' ->
      let sym = tmpsym () in
      pre @ [Def (sym, Alu4 (Add, base, v', 0)); Store (v, sym, 0)]
    | _ -> failwithf "conv_inst: store (ofs)"
    end
  | Llvm.Store2 v ->
    begin match v with
    | Llvm.Var v -> [Store ("#r0", v, 0); Store ("#r0", v, 4)]
    | Llvm.Global v -> [Store ("#r0", "@" ^ v, 0); Store ("#r0", "@" ^ v, 4)]
    | _ -> failwithf "conv_inst: store2"
    end
  | Llvm.CallVoid ("write", v) ->
    begin match v with
    | [Llvm.Int i] -> let sym = tmpsym () in [Def (sym, MovI i); Write sym]
    | [Llvm.Var v] -> [Write v]
    | _ -> failwithf "conv_inst: write"
    end
  | Llvm.CallVoid ("llvm.memset.p0i8.i32", v) ->
    begin match v with
    | [Llvm.Var v; Llvm.Int (-1); Llvm.Var sz; Llvm.Int _; Llvm.Int 0] ->
      [CallVoid ("memset", [v; sz])]
    | [Llvm.Var v; Llvm.Int 0; Llvm.Int sz; Llvm.Int _; Llvm.Int 0] ->
      List.map (fun i -> Store ("#r0", v, i * 4)) (range 0 ((sz + 3) / 4))
    | [Llvm.Global v; Llvm.Int 0; Llvm.Int sz; Llvm.Int _; Llvm.Int 0] ->
      List.map (fun i -> Store ("#r0", "@" ^ v, i * 4)) (range 0 ((sz + 3) / 4))
    | _ -> failwith "conv_inst: memset"
    end
  | Llvm.CallVoid (f, args) ->
    let go v (pre, args) =
      match v with
      | Llvm.Int i -> let sym = tmpsym () in (Def (sym, MovI i) :: pre, sym :: args)
      | Llvm.Float f -> let sym = tmpsym () in (Def (sym, MovF f) :: pre, sym :: args)
      | Llvm.Var v -> (pre, v :: args)
      | Llvm.Global v -> let sym = tmpsym () in (Def (sym, Mov ("@" ^ v)) :: pre, sym :: args) in
    let pre, args = List.fold_right go args ([], []) in
    pre @ [CallVoid (f, args)]

let conv_term = function
  | Llvm.Jump label -> [Jump label]
  | Llvm.Branch (label, b1, b2) -> [Branch (label, b1, b2)]
  | Llvm.Switch (v, dfl, l) -> [Switch (v, dfl, l)]
  | Llvm.Ret v ->
    begin match v with
    | Llvm.Int i -> let sym = tmpsym () in [Def (sym, MovI i); Ret sym]
    | Llvm.Float f -> let sym = tmpsym () in [Def (sym, MovF f); Ret sym]
    | Llvm.Var v -> [Ret v]
    | _ -> failwithf "conv_term: ret"
    end
  | Llvm.RetVoid -> [RetVoid]

let conv_block (label, insts, term) =
  let go inst acc = conv_inst inst :: acc in
  let c = List.fold_right go insts [conv_term term] in
  (label, List.concat c)

let conv_func (name, args, blocks) =
  (name, args, List.map conv_block blocks)

let conv_funcs funcs =
  List.map conv_func funcs
