type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | LET
  | IN
  | LAND
  | PLUS
  | TIMES
  | MINUS
  | DIV
  | AND
  | OR
  | EQ
  | LT
  | IF
  | THEN
  | ELSE
  | LPAR
  | RPAR
  | FUN
  | ARROW
  | SEMISEMI
  | ERROR

val toplevel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.command
