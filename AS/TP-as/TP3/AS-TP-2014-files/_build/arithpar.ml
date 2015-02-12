exception Error

type token = 
  | VAR of (string)
  | TRUE
  | TIMES
  | STRING of (string)
  | PLUS
  | OR
  | OPENP
  | OPEN
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
  | CLOSEP
  | CLOSE
  | ASSIGN
  | AND

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState50
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState38
  | MenhirState36
  | MenhirState34
  | MenhirState32
  | MenhirState29
  | MenhirState20
  | MenhirState18
  | MenhirState16
  | MenhirState14
  | MenhirState6
  | MenhirState5
  | MenhirState4
  | MenhirState2
  | MenhirState0

  
 
 
(** parser *)

open ArithAST

let _eRR =
  Error

let rec _menhir_reduce7 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "Reducing production expr -> assignable \n%!";
    let (_menhir_stack, _menhir_s, a) = _menhir_stack in
    let _v : (ArithAST.t) =                ( a ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 32:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 34:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 36:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 38:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 40:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 42:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 14:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 16:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 20:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 18:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_goto_deb : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    Printf.fprintf Pervasives.stderr "State 47:\n%!";
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        Printf.fprintf Pervasives.stderr "Shifting (EOF) to state 48\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 48:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production start -> deb EOF \n%!";
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (ArithAST.t) =                ( _1 ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 27:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        Printf.fprintf Pervasives.stderr "Accepting\n%!";
        Obj.magic _1
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_assignable : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState50 | MenhirState44 | MenhirState29 | MenhirState42 | MenhirState40 | MenhirState38 | MenhirState36 | MenhirState34 | MenhirState32 | MenhirState2 | MenhirState4 | MenhirState20 | MenhirState16 | MenhirState18 | MenhirState14 | MenhirState5 | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 11:\n%!";
        _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 49:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            Printf.fprintf Pervasives.stderr "Shifting (ASSIGN) to state 50\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 50:\n%!";
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FLOAT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | INT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | MINUS ->
                Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | NOT ->
                Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | OPENP ->
                Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | STRING _v ->
                Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | VAR _v ->
                Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
        | DIVIDE | EOF | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | PLUS | TIMES ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 10:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production expr -> MINUS expr \n%!";
        let ((_menhir_stack, _menhir_s), _, t) = _menhir_stack in
        let _v : (ArithAST.t) =                         ( Un (UMinus,t) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 12:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production expr -> NOT expr \n%!";
        let ((_menhir_stack, _menhir_s), _, t) = _menhir_stack in
        let _v : (ArithAST.t) =                         ( Un (Not,t) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 13:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLOSEP ->
            Printf.fprintf Pervasives.stderr "Shifting (CLOSEP) to state 22\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 22:\n%!";
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> OPENP expr CLOSEP \n%!";
            let ((_menhir_stack, _menhir_s), _, t) = _menhir_stack in
            let _v : (ArithAST.t) =                       ( t ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 15:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production expr -> expr TIMES expr \n%!";
        let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
        let _v : (ArithAST.t) =                         ( Bin (Times, l, r) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 17:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | CLOSE | CLOSEP | EOF | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | OR | PLUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr PLUS expr \n%!";
            let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                         ( Bin (Plus, l, r) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 19:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production expr -> expr DIVIDE expr \n%!";
        let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
        let _v : (ArithAST.t) =                         ( Bin (Divide, l, r) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 21:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | CLOSE | CLOSEP | EOF | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | OR | PLUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr MINUS expr \n%!";
            let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                         ( Bin (Minus, l, r) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 23:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLOSE ->
            Printf.fprintf Pervasives.stderr "Shifting (CLOSE) to state 24\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 24:\n%!";
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprC -> VAR OPEN expr CLOSE \n%!";
            let ((_menhir_stack, _menhir_s, v), _, t) = _menhir_stack in
            let _v : (ArithAST.t) =                          (Index(v,t) ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 9:\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            let s = _v in
            Printf.fprintf Pervasives.stderr "Reducing production assignable -> exprC \n%!";
            let _v : (ArithAST.t) =           ( s ) in
            _menhir_goto_assignable _menhir_env _menhir_stack _menhir_s _v
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState44 | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 31:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 42\n%!";
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 40\n%!";
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 38\n%!";
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 33:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprB -> expr NEQ expr \n%!";
            let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                       ( Bin (Different, l, r) ) in
            _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 35:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprB -> expr LT expr \n%!";
            let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                      ( Bin (LessThan, l, r) ) in
            _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 37:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprB -> expr LEQ expr \n%!";
            let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                        ( Bin (LessThanEq, l, r) ) in
            _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 39:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprB -> expr GT expr \n%!";
            let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                     ( Bin (GreaterThan, l, r) ) in
            _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 41:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprB -> expr GEQ expr \n%!";
            let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                       ( Bin (GreaterThanEq, l, r) ) in
            _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 43:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprB -> expr EQ expr \n%!";
            let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                       ( Bin (Equal, l, r) ) in
            _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 46:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 42\n%!";
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 40\n%!";
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 38\n%!";
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production deb -> expr \n%!";
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (ArithAST.t) =       (_1) in
            _menhir_goto_deb _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 51:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production affect -> assignable ASSIGN expr \n%!";
            let ((_menhir_stack, _menhir_s, a), _, t) = _menhir_stack in
            let _v : (ArithAST.t) =                             (Assign (a, t)) in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 52:\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Printf.fprintf Pervasives.stderr "Reducing production deb -> affect \n%!";
            let _v : (ArithAST.t) =         (_1) in
            _menhir_goto_deb _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_exprB : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 28:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            Printf.fprintf Pervasives.stderr "Shifting (AND) to state 44\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 44:\n%!";
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 26\n%!";
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | FLOAT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | INT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | MINUS ->
                Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | NOT ->
                Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | OPENP ->
                Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | STRING _v ->
                Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | TRUE ->
                Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 25\n%!";
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState44
            | VAR _v ->
                Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
        | OR ->
            Printf.fprintf Pervasives.stderr "Shifting (OR) to state 29\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 29:\n%!";
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 26\n%!";
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | FLOAT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | INT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | MINUS ->
                Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | NOT ->
                Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | OPENP ->
                Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | STRING _v ->
                Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | TRUE ->
                Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 25\n%!";
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | VAR _v ->
                Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production deb -> exprB \n%!";
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (ArithAST.t) =        (_1) in
            _menhir_goto_deb _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 30:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production exprB -> exprB OR exprB \n%!";
        let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
        let _v : (ArithAST.t) =                       ( Bin (Or, l, r) ) in
        _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 45:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production exprB -> exprB AND exprB \n%!";
        let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
        let _v : (ArithAST.t) =                         ( Bin (And, l, r) ) in
        _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        ();
        Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
        assert false

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    Printf.fprintf Pervasives.stderr "Lookahead token is now %s (%d-%d)\n%!" (_menhir_print_token _tok) _menhir_env._menhir_startp.Lexing.pos_cnum _menhir_env._menhir_endp.Lexing.pos_cnum;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 1:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | OPEN ->
        Printf.fprintf Pervasives.stderr "Shifting (OPEN) to state 2\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 2:\n%!";
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FLOAT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | INT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | NOT ->
            Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | OPENP ->
            Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | VAR _v ->
            Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | AND | CLOSE | CLOSEP | DIVIDE | EOF | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | OR | PLUS | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production expr -> VAR \n%!";
        let (_menhir_stack, _menhir_s, s) = _menhir_stack in
        let _v : (ArithAST.t) =                         ( Var s ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | ASSIGN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production assignable -> VAR \n%!";
        let (_menhir_stack, _menhir_s, s) = _menhir_stack in
        let _v : (ArithAST.t) =         ( Var s ) in
        _menhir_goto_assignable _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 25:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    Printf.fprintf Pervasives.stderr "Reducing production exprB -> TRUE \n%!";
    let _v : (ArithAST.t) =          (True) in
    _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 3:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let s = _v in
    Printf.fprintf Pervasives.stderr "Reducing production expr -> STRING \n%!";
    let _v : (ArithAST.t) =                         ( String s ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 4:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 5:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 6:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 7:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    Printf.fprintf Pervasives.stderr "Reducing production expr -> INT \n%!";
    let _v : (ArithAST.t) =                         ( Int i ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 8:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let f = _v in
    Printf.fprintf Pervasives.stderr "Reducing production expr -> FLOAT \n%!";
    let _v : (ArithAST.t) =                         ( Float f ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 26:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    Printf.fprintf Pervasives.stderr "Reducing production exprB -> FALSE \n%!";
    let _v : (ArithAST.t) =           (False) in
    _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v

and _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | AND ->
        "AND"
    | ASSIGN ->
        "ASSIGN"
    | CLOSE ->
        "CLOSE"
    | CLOSEP ->
        "CLOSEP"
    | DIVIDE ->
        "DIVIDE"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | FALSE ->
        "FALSE"
    | FLOAT _ ->
        "FLOAT"
    | GEQ ->
        "GEQ"
    | GT ->
        "GT"
    | INT _ ->
        "INT"
    | LEQ ->
        "LEQ"
    | LT ->
        "LT"
    | MINUS ->
        "MINUS"
    | NEQ ->
        "NEQ"
    | NOT ->
        "NOT"
    | OPEN ->
        "OPEN"
    | OPENP ->
        "OPENP"
    | OR ->
        "OR"
    | PLUS ->
        "PLUS"
    | STRING _ ->
        "STRING"
    | TIMES ->
        "TIMES"
    | TRUE ->
        "TRUE"
    | VAR _ ->
        "VAR"

and start : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ArithAST.t) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    Printf.fprintf Pervasives.stderr "Lookahead token is now %s (%d-%d)\n%!" (_menhir_print_token _tok) lexbuf.Lexing.lex_start_p.Lexing.pos_cnum lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum;
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = 4611686018427387903;
      } in
    Obj.magic (let _menhir_stack = () in
    Printf.fprintf Pervasives.stderr "State 0:\n%!";
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 26\n%!";
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 25\n%!";
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



