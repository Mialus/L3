open List
open Helper

(******************************)

type op = Plus | Times | Minus | Divide |
  Equal | Different | LessThan | GreaterThan | LessThanEq | GreaterThanEq |
  And | Or | Assign
  
type uop = UMinus | Not
type t =
  | Int of int
  | Float of float
  | String of string
  | Var of string
  | Bin of op * t * t
  | Un  of uop * t
  | Dummy of string * t list
  | True
  | False 
  | Index of string * t
  | Assign of t * t
  | Stmts of t list
  | While of t * t
  | For of t * t * t * t 
  | Do of t * t
  | If of t * t
  | IfElse of t * t * t
  | Tern of t * t * t

let str_of_op = function
  | Plus -> "+"
  | Times -> "*"
  | Minus -> "-"
  | Divide -> "/"
  | Equal -> "=="
  | Different -> "<>"
  | LessThan -> "<"
  | GreaterThan -> ">"
  | LessThanEq -> "<="
  | GreaterThanEq  -> ">="
  | And -> "&&"
  | Or -> "||"
  
let str_of_uop = function UMinus -> "-" | Not -> "!"

let rec dot = function
  | Int i -> Dot.N ("i:" ^ soi i, [])
  | String s -> Dot.N ("s:" ^ String.escaped s, [])
  | Var s -> Dot.N ("v:" ^ String.escaped s, [])
  | Float f -> Dot.N ("f:" ^ sof f, [])
  | Bin (o,l,r) -> Dot.N (str_of_op o, [dot l; dot r])
  | Un (o,t) -> Dot.N (str_of_uop o, [dot t])
  | Dummy (s,l) -> Dot.N (spf "<%s>" s, map dot l)
  | True -> Dot.N("true",[])
  | False -> Dot.N("false",[])
  | Index (id,x) -> Dot.N (id ^ "[.]", [dot x])
  | Assign (id,t) -> Dot.N (":=", [dot id; dot t])
  | Stmts l -> Dot.N ("<stmts>", map dot l)
  | While (l,r) -> Dot.N("While",[dot l; dot r])
  | For (s,b,t,ter) -> Dot.N("for",[dot s; dot b; dot t; dot ter])
  | Do  (s,b)  -> Dot.N("do",[dot s; dot b])
  | If (b,ter) -> Dot.N("if", [dot b; dot ter])
  | IfElse (l,m,r) -> Dot.N ("if else", [dot l; dot m; dot r])
  | Tern (l,m,r) -> Dot.N ( "?",[ dot l; dot m; dot r])
