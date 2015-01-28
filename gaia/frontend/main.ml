
open Mylib
open Printf
open Lexing

let parse lexbuf =
  let error msg =
    let pos = lexbuf.lex_start_p in
    eprintf "%s:%d:%d: error: %s\n"
      pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) msg;
    exit 1 in
  try Parser.top Lexer.token lexbuf with
  | Failure "parse" -> error (sprintf "parse error near '%s'" (lexeme lexbuf))
  | Failure msg -> error msg

let () =
  let libname = "lib/urnslib.ml" in
  let libpath = Filename.concat (Filename.dirname Sys.argv.(0)) libname in
  let inputs = ref [] in
  let outfile = ref "" in
  let nolib = ref false in
  let inlineall = ref false in
  let speclist = [
    ("-o", Arg.Set_string outfile, "<file> Set output file name");
    ("-no-lib", Arg.Set nolib, " Do not link " ^ libname);
    ("-always-inline", Arg.Set inlineall, " Specify \"alwaysinline\" attribute")
  ] in
  Arg.parse (Arg.align speclist)
    (fun fname -> inputs := fname :: !inputs)
    (sprintf "Usage: %s [options] file..." Sys.argv.(0));
  if !inputs = [] && !outfile = "" then nolib := true;
  inputs := if !inputs = [] then ["<stdin>"] else List.rev !inputs;
  inputs := if !nolib then !inputs else libpath :: !inputs;
  let read fname =
    let ic = if fname = "<stdin>" then stdin else open_in fname in
    sprintf "#file %s/\n%s" (Filename.basename fname) (input_all ic) in
  let prog = String.concat "" (List.map read !inputs) in
  let ast = parse (Lexing.from_string prog) in
  let globenv = TypeCheck.type_check ast in
  let oc =
    if !outfile <> "" then open_out !outfile else
    if !inputs = ["<stdin>"] then stdout else
    open_out (Filename.chop_extension (List.hd (List.rev !inputs)) ^ ".ll") in
  Emit.emit oc globenv ast !inlineall

