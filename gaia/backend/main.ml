
open Util
open Printf
open Lexing

let parse fname =
  let ic = open_in fname in
  let lexbuf = from_channel ic in
  let error msg =
    let pos = lexbuf.lex_start_p in
    eprintf "%s:%d:%d: error: %s\n"
      fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) msg;
    exit 1 in
  try Parser.top Lexer.token lexbuf with
  | Failure "parse" -> error (sprintf "parse error near '%s'" (lexeme lexbuf))
  | Failure msg -> error msg

let () =
  ignore (parse Sys.argv.(1));
  print_endline "finished"
