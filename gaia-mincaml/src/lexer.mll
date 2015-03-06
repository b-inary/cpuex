{
  open Parser
  open Type
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+  { token lexbuf }
| "(*"    { comment lexbuf; token lexbuf }
| '('     { LPAREN }
| ')'     { RPAREN }
| "true"  { BOOL true }
| "false" { BOOL false }
| "not"   { NOT }
| "xor"   { XOR }
| digit+  { INT (int_of_string (Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
          { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
| '+'     { PLUS }
| '-'     { MINUS }
| '*'     { AST }
| '/'     { SLASH }
| "+."    { PLUS_DOT }
| "-."    { MINUS_DOT }
| "*."    { AST_DOT }
| "/."    { SLASH_DOT }
| '='     { EQUAL }
| "<>"    { LESS_GREATER }
| "<="    { LESS_EQUAL }
| ">="    { GREATER_EQUAL }
| '<'     { LESS }
| '>'     { GREATER }
| "if"    { IF }
| "then"  { THEN }
| "else"  { ELSE }
| "let"   { LET }
| "in"    { IN }
| "rec"   { REC }
| ','     { COMMA }
| '_'     { IDENT (Id.gentmp Type.Unit) }
| "create_array"
          { ARRAY_CREATE }
| "Array.create"
          { ARRAY_CREATE }
| '.'     { DOT }
| "<-"    { LESS_MINUS }
| ';'     { SEMICOLON }
| eof     { EOF }
| "fequal"  { FEQUAL }
| "fless"   { FLESS }
| "fispos"  { FISPOS }
| "fisneg"  { FISNEG }
| "fiszero" { FISZERO }
| "fhalf"   { FHALF }
| "fsqr"    { FSQR }
| "fabs"    { FABS }
| "fneg"    { FNEG }
| "float_of_int" { ITOF }
| "int_of_float" { FTOI }
| "floor"   { FLOOR }
| lower (digit|lower|upper|'_')*
          { IDENT (Lexing.lexeme lexbuf) }
| _
          { failwith
              (Printf.sprintf "\nerror: unknown token %s near characters %d-%d\n"
                (Lexing.lexeme lexbuf)
                (Lexing.lexeme_start lexbuf)
                (Lexing.lexeme_end lexbuf)) }

and comment = parse
| "*)"    { () }
| "(*"    { comment lexbuf; comment lexbuf }
| eof     { Format.eprintf "warning: unterminated comment@." }
| _       { comment lexbuf }

