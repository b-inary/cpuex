
let limit = ref 1000

(* 最適化処理をくりかえす *)
let rec iter n e =
  if !Typing.lv >= 3 then Format.eprintf "[info] iteration: %d@." (!limit - n + 1);
  if n = 0 then (if !Typing.lv >= 1 then Format.eprintf "[info] iteration terminated@."; e) else
  let e' = e |> Beta.f |> Assoc.f |> Inline.f |> ConstFold.f |> Elim.f in
  if e = e' then (if !Typing.lv >= 2 then Format.eprintf "[info] iteration finished (%d times)@." (!limit - n + 1) ; e) else
  iter (n - 1) e'

(* バッファをコンパイルしてチャンネルへ出力する *)
let lexbuf l =
  Id.counter := 0;
  Typing.extenv := M.empty;
  RegAlloc.f (Virtual.f (Closure.f
    (iter !limit (Alpha.f (KNormal.f
      (Typing.f (Parser.exp Lexer.token l)))))))

(* ファイルをコンパイルしてファイルに出力する *)
let file f =
  let inchan = if f = "stdinout" then stdin else open_in f in
  try
    let ret = lexbuf (Lexing.from_channel inchan) in
    let outchan =
      if f = "stdinout" then stdout else
      open_out (String.sub f 0 (String.rindex f '.') ^ ".s") in
    Emit.f outchan ret;
    close_in inchan;
    close_out outchan
  with e -> (close_in inchan; raise e)

(* ここからコンパイラの実行が開始される *)
let () =
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int (fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int (fun i -> limit := i), "maximum number of optimizations iterated");
     ("-log", Arg.Int (fun i -> Typing.lv := i), "log level (0 - 3)")]
    (fun s -> files := !files @ [s])
    (Printf.sprintf "usage: %s [options] file..." Sys.argv.(0));
  if !files = [] then file "stdinout" else
  List.iter (fun f -> ignore (file f)) !files
