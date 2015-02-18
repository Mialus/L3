%{
 
 
(** parser *)

open ArithAST

%}

%token EOF
  PLUS TIMES MINUS DIVIDE TRUE FALSE ENT FL VIR
  NEQ EQ LEQ GEQ LT GT WHILE FOR DO PE MM ME PP IF ELSE DEUP INTE 
  AND OR NOT CLOSEP OPENP OPEN CLOSE ASSIGN  PV ACOPEN ACOFER
%token<int> INT
%token<float> FLOAT
%token<string> STRING VAR 

%left INTE
%left AND OR
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE 
%nonassoc UMINUS /* virtual token */
%nonassoc NOT


%start <ArithAST.t> start

%%

start: start2 EOF { $1 } /* YACC-style indexing $1, $2, etc */

start2:
| expr { $1 }
| exprC { $1 }
| terminated_stmt { $1 }
| declare {$1}

declare:
| ENT a=createInt {a}
| FL b=createInt {b}

createFloat:
|c=float_inner { Stmts c }

float_inner:
| { [] }
| s=exprD VIR ss=createFloat { s::ss }
| s=exprD PV ss=int_inner { s::ss }
| s=exprD ss=int_inner { s::ss }

createInt:
|c=int_inner { Stmts c }

int_inner:
| { [] } 
| s=exprD PV ss=int_inner { s::ss }
| s=exprD ss=int_inner { s::ss }

expr:
| i=INT                 { Int i }
| f=FLOAT               { Float f }
| s=STRING              { String s }
| s=VAR                 { Var s }
| l=expr PLUS r=expr    { Bin (Plus, l, r) }
| l=expr TIMES r=expr   { Bin (Times, l, r) }
| l=expr MINUS r=expr   { Bin (Minus, l, r) }
| l=expr DIVIDE r=expr  { Bin (Divide, l, r) }
| OPENP l=expr PLUS r=expr CLOSEP   { Bin (Plus, l, r) }
| OPENP l=expr TIMES r=expr CLOSEP  { Bin (Times, l, r) }
| OPENP l=expr MINUS r=expr CLOSEP  { Bin (Minus, l, r) }
| OPENP l=expr DIVIDE r=expr CLOSEP { Bin (Divide, l, r) }
| e=exprB         { e }
| MINUS t=expr          { Un (UMinus,t) }       %prec UMINUS
| NOT   t=expr          { Un (Not,t) }
| v=VAR OPEN t=expr CLOSE	{ Index (v,t) }
| t=expr INTE e=expr DEUP r=expr    { Tern(t,e,r)}


exprD:
| a=assignable ASSIGN b=expr 	{ EntVal(a,b)}
| a=assignable {EntValU(a) }

stmts_inner:
| { [] } 
| s=exprC PV ss=stmts_inner { s::ss }

stmts: l=stmts_inner { Stmts l }

loop:
| ACOPEN s=stmts ACOFER     { s }

terminated_stmt: 
| l=loop                                    { l }
| s=exprC PV 								{ s }
| WHILE r=expr t=terminated_stmt { While(r,t) }
| FOR OPENP a=exprC PV e=expr PV u=exprC CLOSEP t=terminated_stmt { For(a,e,u,t) }
| IF e=expr t=terminated_stmt ELSE td=terminated_stmt { IfElse(e,t,td) } 
| IF e=expr t=terminated_stmt { If(e,t) } 
| ACOPEN s=stmts ACOFER							{ s }

assignable:
|v=VAR { Var v }
|id=VAR OPEN t=expr CLOSE	{ Index (id,t) } 

exprC:
| DO t=terminated_stmt WHILE e=expr { Do(t,e) }
| id=assignable ASSIGN	t=expr 		{  Assign (id,t)}
| id=assignable PP	 		{ Assign (id,Bin (Plus, id,Int 1))}
| id=assignable PE	t=expr 	{ Assign (id,Bin (Plus, id, t))}
| id=assignable ME  t=expr 	{ Assign (id,Bin (Minus, id,  t))}
| id=assignable MM		 	{ Assign (id,Bin (Minus, id, Int 1))}

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
| OPENP b=exprB CLOSEP		{b}
