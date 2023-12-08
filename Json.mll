{
  open Lexing
  exception SyntaxError of string
}

rule token = parse
  | '"'        { string lexbuf }
  | '{'        { LBRACE }
  | '}'        { RBRACE }
  | '['        { LBRACKET }
  | ']'        { RBRACKET }
  | ','        { COMMA }
  | ':'        { COLON }
  | [ '\t' ' ' '\n' '\r' ]   { token lexbuf }
  | eof        { EOF }
  | _          { raise (SyntaxError "Unexpected character") }

and string = parse
  | '"'        { STRING (Lexing.lexeme lexbuf) }
  | '\\' '"'   { string lexbuf }
  | _ as c     { string lexbuf }

