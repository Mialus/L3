%{
 
 
(** parser *)

open ArithAST

%}

%token EOF
  PLUS TIMES MINUS DIVIDE
  NEQ EQ LEQ GEQ LT GT
  AND OR NOT TRUE FALSE INDEX
%token<int> INT
%token<float> FLOAT
%token<string> STRING VAR

%left AND OR
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS /* virtual token */
%nonassoc NOT

%start <ArithAST.t> start 

%%


start: exprB EOF { $1 } /* YACC-style indexing $1, $2, etc */


expr:
| i=INT                 { Int i }
| f=FLOAT               { Float f }
| s=STRING              { String s }
| s=VAR                 { Var s }
| l=expr PLUS r=expr    { Bin (Plus, l, r) }
| l=expr TIMES r=expr   { Bin (Times, l, r) }
| l=expr MINUS r=expr   { Bin (Minus, l, r) }
| l=expr DIVIDE r=expr  { Bin (Divide, l, r) }
| MINUS t=expr          { Un (UMinus,t) }       %prec UMINUS
| NOT   t=expr          { Un (Not,t) }
| 


exprB:

| l=expr EQ r=expr    { Bin (Equal, l, r) }
| l=expr NEQ r=expr   { Bin (Different, l, r) }
| l=expr LT r=expr   { Bin (LessThan, l, r) }
| l=expr GT r=expr  { Bin (GreaterThan, l, r) }
| l=expr LEQ r=expr    { Bin (LessThanEq, l, r) }
| l=expr GEQ r=expr   { Bin (GreaterThanEq, l, r) }
| l=exprB AND r=exprB   { Bin (And, l, r) }
| l=exprB OR r=exprB  { Bin (Or, l, r) }
| TRUE			{True}
| FALSE			{False}

