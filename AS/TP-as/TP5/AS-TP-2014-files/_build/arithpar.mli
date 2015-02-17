exception Error

type token = 
  | WHILE
  | VAR of (string)
  | TRUE
  | TIMES
  | STRING of (string)
  | PV
  | PP
  | PLUS
  | PE
  | OR
  | OPENP
  | OPEN
  | NOT
  | NEQ
  | MM
  | MINUS
  | ME
  | LT
  | LEQ
  | INTE
  | INT of (int)
  | IF
  | GT
  | GEQ
  | FOR
  | FLOAT of (float)
  | FALSE
  | EQ
  | EOF
  | ELSE
  | DO
  | DIVIDE
  | DEUP
  | CLOSEP
  | CLOSE
  | ASSIGN
  | AND
  | ACOPEN
  | ACOFER


val start: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ArithAST.t)