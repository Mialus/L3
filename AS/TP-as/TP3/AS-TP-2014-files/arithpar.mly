%{
 
 
(** parser *)

open ArithAST

%}

%token EOF
  PLUS TIMES MINUS DIVIDE
  NEQ EQ LEQ GEQ LT GT
  AND OR NOT TRUE FALSE OPEN CLOSE 
  OPENP CLOSEP ASSIGN FINAL

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


start: deb EOF { $1 } /* YACC-style indexing $1, $2, etc */

deb:
|stmts {$1}
|expr {$1}
|affect {$1}
|exprB {$1}


affect:
|a=assignable ASSIGN t=expr	{Assign (a, t)}

stmts_inner:
| { [] }
| s=stmt PV r=expr_inner { s::r }

expr:
|a=assignable		{ a }
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
| OPENP	t=expr CLOSEP	{ t }

stmts:
| l=stmts_inner {Stmts l}

assignable:
| s=exprC { s }
| s=VAR { Var s }

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

exprC:
|v=VAR OPEN t=expr CLOSE {Index(v,t) }

