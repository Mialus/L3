exception Error

type token = 
  | VAR of (string)
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
  | EQ
  | EOF
  | DIVIDE
  | AND


val start1: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ArithAST.t)
val start: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ArithAST.t)