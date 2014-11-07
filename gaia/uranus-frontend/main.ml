
let () =
  print_endline @@ Syntax.string_of_ast @@ Parser.top Lexer.token @@
    Lexing.from_channel stdin
