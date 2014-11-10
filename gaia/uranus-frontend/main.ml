
let parse buf =
  let error msg =
    let pos = buf.Lexing.lex_start_p in
    Printf.eprintf "%s:%d:%d: error: %s\n"
      pos.Lexing.pos_fname
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
      msg;
    exit 1 in
  try Parser.top Lexer.token buf with
      Failure "parse" -> error ("parse error near '" ^ Lexing.lexeme buf ^ "'")
    | Failure msg -> error msg

let () =
  let argc = Array.length Sys.argv in
  if argc = 1 then
    let ast = parse (Lexing.from_channel stdin) in
    print_endline (Syntax.string_of_ast ast)
  else
    let rec go n =
      if n = argc then "" else
      let s = Sys.argv.(n) in
      let fname =
        try
          let i = String.rindex s '/' in
          String.sub s (i + 1) (String.length s - i - 1)
        with Not_found -> s in
      let ic = open_in Sys.argv.(n) in
      let content = really_input_string ic (in_channel_length ic) in
      "#file " ^ fname ^ "/" ^ content ^ go (n + 1) in
    let ast = parse (Lexing.from_string (go 1)) in
    print_endline (Syntax.string_of_ast ast)
