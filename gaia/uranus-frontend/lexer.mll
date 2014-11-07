{
open Parser
let counter = ref 0
let new_ident () = incr counter; "Tmp" ^ string_of_int !counter
}

let space = [' ' '\t' '\n' '\r']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let digit = ['0'-'9']
let dec   = digit+
let hex   = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let oct   = '0' ['o' 'O'] ['0'-'7']+
let bin   = '0' ['b' 'B'] ['0'-'1']+
let int_lit = dec | hex | oct | bin

rule token = parse
    space+  { token lexbuf }
  | eof     { EOF }
  | "(*"    { comment lexbuf; token lexbuf }
  | '('     { LPAR }
  | ')'     { RPAR }
  | "not"   { NOT }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { AST }
  | '/'     { SLASH }
  | "+."    { PLUSDOT }
  | "-."    { MINUSDOT }
  | "*."    { ASTDOT }
  | "/."    { SLASHDOT }
  | '='     { EQUAL }
  | "<>"    { NOTEQ }
  | '<'     { LESS }
  | "<="    { LESSEQ }
  | '>'     { GREATER }
  | ">="    { GREATEREQ }
  | "if"    { IF }
  | "then"  { THEN }
  | "else"  { ELSE }
  | "let"   { LET }
  | "in"    { IN }
  | "rec"   { REC }
  | ','     { COMMA }
  | ';'     { SEMI }
  | '.'     { DOT }
  | "<-"    { ASSIGN }
  | "create_array" { MAKEARRAY }
  | "fabs"  { FABS }
  | "true"  { BOOL true }
  | "false" { BOOL false }
  | int_lit as lxm { INT (int_of_string lxm) }
  | digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)? as lxm
            { FLOAT (float_of_string lxm) }
  | '_'     { IDENT (new_ident ()) }
  | lower (digit | lower | upper | ['_' '\''])* as lxm { IDENT lxm }
  | "#Read"       { READ }
  | "#Write"      { WRITE }
  | "#ItoF"       { ITOF }
  | "#FtoI"       { FTOI }
  | "#Floor"      { FLOOR }
  | "#CastInt"    { CASTINT }
  | "#CastFloat"  { CASTFLT }

and comment = parse
  | "*)"    { () }
  | "(*"    { comment lexbuf; comment lexbuf }
  | eof     { Format.eprintf "warning: unterminated comment@." }
  | _       { comment lexbuf }

