exception Error

type token = 
  | VAR of (string)
  | TRUE
  | TIMES
  | STRING of (string)
  | PLUS
  | OR
  | NOT
  | NEQ
  | MINUS
  | LT
  | LEQ
  | INT of (int)
  | GT
  | GEQ
  | FLOAT of (float)
  | FALSE
  | EQ
  | EOF
  | DIVIDE
  | AND


val start: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ArithAST.t)