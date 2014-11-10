{
open Parser
open Lexing

let counter = ref (-1)
let new_ident () = incr counter; "Tmp" ^ string_of_int !counter

let init lexbuf fname =
    lexbuf.lex_curr_p <- {
        pos_fname = fname;
        pos_lnum = 1;
        pos_bol = 0;
        pos_cnum = 0;
    }
}


let space = [' ' '\t' '\r']
let lower = ['a'-'z' '_']
let upper = ['A'-'Z']
let digit = ['0'-'9']
let ident = lower (digit | lower | upper | '\'')*
let dec = digit+
let hex = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let oct = '0' ['o' 'O'] ['0'-'7']+
let bin = '0' ['b' 'B'] ['0'-'1']+
let inum = dec | hex | oct | bin
let fnum = digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?

rule token = parse
    space+          { token lexbuf }
  | '\n'            { new_line lexbuf; token lexbuf }
  | "(*"            { comment lexbuf; token lexbuf }
  | "true"          { BOOL true }
  | "false"         { BOOL false }
  | inum as lxm     { INT (int_of_string lxm) }
  | fnum as lxm     { FLOAT (float_of_string lxm) }
  | '('             { LPAR }
  | ')'             { RPAR }
  | "not"           { NOT }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { AST }
  | '/'             { SLASH }
  | "+."            { PLUSDOT }
  | "-."            { MINUSDOT }
  | "*."            { ASTDOT }
  | "/."            { SLASHDOT }
  | "fabs"          { FABS }
  | '='             { EQUAL }
  | "<>"            { NOTEQ }
  | '<'             { LESS }
  | "<="            { LESSEQ }
  | '>'             { GREATER }
  | ">="            { GREATEREQ }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "let"           { LET }
  | "in"            { IN }
  | "rec"           { REC }
  | ','             { COMMA }
  | ';'             { SEMI }
  | ";;"            { SEMISEMI }
  | '.'             { DOT }
  | "<-"            { ASSIGN }
  | "create_array"  { MAKEARRAY }
  | '_'             { IDENT (new_ident ()) }
  | ident as lxm    { IDENT lxm }
  | "#read"         { READ }
  | "#write"        { WRITE }
  | "#itof"         { ITOF }
  | "#ftoi"         { FTOI }
  | "#floor"        { FLOOR }
  | "#castint"      { CASTINT }
  | "#castfloat"    { CASTFLT }
  | "#file " ([^'/']+ as fname) '/' { init lexbuf fname; token lexbuf }
  | eof             { EOF }
  | _               { failwith ("illegal token '" ^ lexeme lexbuf ^ "'") }

and comment = parse
    "(*"    { comment lexbuf; comment lexbuf }
  | "*)"    {}
  | '\n'    { new_line lexbuf; comment lexbuf }
  | eof     { failwith "unterminated comment" }
  | _       { comment lexbuf }

