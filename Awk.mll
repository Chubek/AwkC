let ws = [' ' '\t' '\r']

let dec_digit = ['0'-'9']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let oct_digit = ['0'-'7']
let bin_digit = ['0' '1']

let digit = dec_digit | hex_digit | oct_digit | bin_digit

let newline = '\n'
let newline_or_semicolon = '\n' | ';'

let ident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | ws+                  { token lexbuf }
  | newline              { NEWLINE }
  | newline_or_semicolon { NEWLINE }
  | "BEGIN"              { BEGIN }
  | "END"                { END }
  | "getline"            { GETLINE }
  | "break"              { BREAK }
  | "continue"           { CONTINUE }
  | "delete"             { DELETE }
  | "do"                 { DO }
  | "else"               { ELSE }
  | "exit"               { EXIT }
  | "for"                { FOR }
  | "function"           { FUNCTION }
  | "if"                 { IF }
  | "in"                 { IN }
  | "next"               { NEXT }
  | "print"              { PRINT }
  | "printf"             { PRINTF }
  | "return"             { RETURN }
  | "while"              { WHILE }
  | [a-zA-Z_] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { NAME(id) }
  | digit+ as num       { NUMBER(num) }
  | digit* [eE\.] digit+ as rational { FLOAT(rational) }
  | '"' [^'"']* '"'      { STRING (String.sub (Lexing.lexeme lexbuf) 1 (String.length (Lexing.lexeme lexbuf) - 2)) }
  | "//" [^'\n']* '\n'   { token lexbuf }
  | "/" [^'/'] "/" as ere { ERE(ere)  }

and atan2 = "atan2"      { BUILTIN_FUNC_NAME }
and cos = "cos"          { BUILTIN_FUNC_NAME }
and sin = "sin"          { BUILTIN_FUNC_NAME }
and exp = "exp"          { BUILTIN_FUNC_NAME }
and log = "log"          { BUILTIN_FUNC_NAME }
and sqrt = "sqrt"        { BUILTIN_FUNC_NAME }
and int = "int"          { BUILTIN_FUNC_NAME }
and rand = "rand"        { BUILTIN_FUNC_NAME }
and srand = "srand"      { BUILTIN_FUNC_NAME }
and gsub = "gsub"        { BUILTIN_FUNC_NAME }
and index = "index"      { BUILTIN_FUNC_NAME }
and length = "lengh"     { BUILTIN_FUNC_NAME }
and match_ = "match"     { BUILTIN_FUNC_NAME }
and split = "split"      { BUILTIN_FUNC_NAME }
and sprintf = "sprintf"  { BUILTIN_FUNC_NAME }
and sub = "sub"          { BUILTIN_FUNC_NAME }
and substr = "substr"    { BUILTIN_FUNC_NAME }
and tolower = "tolower"  { BUILTIN_FUNC_NAME }
and toupper = "toupper"  { BUILTIN_FUNC_NAME }
and close = "close"      { BUILTIN_FUNC_NAME }
and system = "system"    { BUILTIN_FUNC_NAME }

