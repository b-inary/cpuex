{
open Util
open Parser
open Lexing
}

let space = [' ' '\t' '\r']
let digit = ['0'-'9']
let ident = ['a'-'z' 'A'-'Z' '0'-'9' '_' '.' '$' '-']+
let sci = digit+ '.' digit+ 'e' ['+' '-'] digit+
let hex = "0x" ['0'-'9' 'A'-'F']+

rule comment = parse
  | '\n'  { new_line lexbuf; EOL }
  | eof   { EOF }
  | _     { comment lexbuf }

and token = parse
  | space+  { token lexbuf }
  | '\n'    { new_line lexbuf; EOL }
  | ';'     { comment lexbuf }
  | eof     { EOF }

  | '='       { EQUAL }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '['       { LBRACKET }
  | ']'       { RBRACKET }
  | ','       { COMMA }
  | "define"  { DEFINE }
  | "label"   { LABEL }

  | "i1"    { I1 }
  | "i8"    { I8 }
  | "i32"   { I32 }
  | "i64"   { I64 }
  | "float" { FLT }
  | "void"  { VOID }
  | '{'     { LBRACE }
  | '}'     { RBRACE }
  | '*'     { AST }
  | 'x'     { X }

  | "br"        { BR }
  | "ret"       { RET }
  | "switch"    { SWITCH }
  | "add"       { ADD }
  | "sub"       { SUB }
  | "shl"       { SHL }
  | "lshr"      { SHR }
  | "and"       { AND }
  | "or"        { OR }
  | "xor"       { XOR }
  | "fadd"      { FADD }
  | "fsub"      { FSUB }
  | "fmul"      { FMUL }
  | "fdiv"      { FDIV }
  | "load"      { LOAD }
  | "store"     { STORE }
  | "getelementptr" { GEP }
  | "bitcast"   { BITCAST }
  | "to"        { TO }
  | "sitofp"    { SITOFP }
  | "icmp"      { ICMP }
  | "fcmp"      { FCMP }
  | "select"    { SELECT }
  | "call"      { CALL }
  | "phi"       { PHI }
  | "eq"        { EQ }
  | "oeq"       { EQ }
  | "ueq"       { EQ }
  | "ne"        { NE }
  | "one"       { NE }
  | "une"       { NE }
  | "slt"       { LT }
  | "olt"       { LT }
  | "ult"       { LT }
  | "sle"       { LE }
  | "ole"       { LE }
  | "ule"       { LE }
  | "sgt"       { GT }
  | "ogt"       { GT }
  | "ugt"       { GT }
  | "sge"       { GE }
  | "oge"       { GE }
  | "uge"       { GE }

  | "true"              { INT 1 }
  | "false"             { INT 0 }
  | "null"              { INT 0 }
  | '-'? digit+ as lxm  { INT (int_of_string lxm) }
  | '-'? sci as lxm     { FLOAT (float_of_string lxm) }
  | hex as lxm          { FLOAT (Int64.float_of_bits (Int64.of_string lxm)) }
  | '%' (ident as lxm)  { VAR lxm }
  | '@' (ident as lxm)  { GLOBAL lxm }
  | "; <label>:" (ident as lxm) { DEFLABEL lxm }
  | (ident+ as lxm) ':' { DEFLABEL lxm }

  | "fast"
  | "fastcc"
  | "global"
  | "inbounds"
  | "internal"
  | "noalias"
  | "nocapture"
  | "nounwind"
  | "private"
  | "readonly"
  | "tail"
  | "undef"
  | "unnamed_addr"
  | ", align " digit+
  | '#' ident+
    { token lexbuf }
  | "attributes"
  | "declare"
  | "target"
    { comment lexbuf }

  | _ { failwithf "illegal token `%s'" (lexeme lexbuf) }
