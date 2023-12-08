%{
open Json_ast
%}

%token STRING

%start json
%type <Json_ast.json> json

%%

json:
  | object_            { $1 }
  | array              { $1 }

object_:
  | '{' '}'            { JsonObject [] }
  | '{' members '}'    { JsonObject $2 }

members:
  | pair               { [$1] }
  | pair ',' members   { $1 :: $3 }

pair:
  | STRING ':' STRING { (fst $1, JsonString (snd $3)) }

array:
  | '[' ']'            { JsonArray [] }
  | '[' elements ']'   { JsonArray $2 }

elements:
  | json               { [$1] }
  | json ',' elements  { $1 :: $3 }
  
%token EOF

%%


let parse_json lexbuf =
  try
    json Lexer.token lexbuf
  with
  | Parsing.Parse_error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.eprintf "Syntax error at line %d, position %d\n"
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1
  | Lexer.SyntaxError msg ->
      Printf.eprintf "Lexer error: %s\n" msg;
      exit 1
  | _ ->
      Printf.eprintf "An error occurred.\n";
      exit 1

