
open Mylib
open Printf

let parse buf =
  let error msg =
    let pos = buf.Lexing.lex_start_p in
    eprintf "%s:%d:%d: error: %s\n"
      pos.Lexing.pos_fname
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
      msg;
    exit 1 in
  try Parser.top Lexer.token buf with
      Failure "parse" -> error (sprintf "parse error near '%s'" (Lexing.lexeme buf))
    | Failure msg -> error msg

let () =
  let argv = Sys.argv |> Array.to_list |> List.tl in
  let inputs = "uranuslib.ml" :: (if argv = [] then ["<stdin>"] else argv) in
  let read fname =
    let ic = if fname = "<stdin>" then stdin else open_in fname in
    let pos = try String.rindex fname '/' with Not_found -> -1 in
    let fname = String.sub fname (pos + 1) (String.length fname - pos - 1) in
    sprintf "#file %s/\n%s" fname (input_all ic) in
  let content = String.concat "" (List.map read inputs) in
  let ast = parse (Lexing.from_string content) in
  let global_env = TypeCheck.type_check ast in
  Emit.emit global_env ast

