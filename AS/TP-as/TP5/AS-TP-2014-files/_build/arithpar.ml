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

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState100
  | MenhirState93
  | MenhirState92
  | MenhirState88
  | MenhirState85
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState76
  | MenhirState75
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState68
  | MenhirState67
  | MenhirState63
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState54
  | MenhirState53
  | MenhirState51
  | MenhirState50
  | MenhirState48
  | MenhirState46
  | MenhirState40
  | MenhirState39
  | MenhirState38
  | MenhirState37
  | MenhirState36
  | MenhirState35
  | MenhirState34
  | MenhirState33
  | MenhirState32
  | MenhirState31
  | MenhirState30
  | MenhirState29
  | MenhirState28
  | MenhirState27
  | MenhirState26
  | MenhirState25
  | MenhirState24
  | MenhirState23
  | MenhirState22
  | MenhirState20
  | MenhirState19
  | MenhirState18
  | MenhirState16
  | MenhirState15
  | MenhirState13
  | MenhirState8
  | MenhirState7
  | MenhirState6
  | MenhirState3
  | MenhirState1
  | MenhirState0

  
 
 
(** parser *)

open ArithAST

let _eRR =
  Error

let rec _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 79:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    Printf.fprintf Pervasives.stderr "Reducing production terminated_stmt -> exprC PV \n%!";
    let (_menhir_stack, _menhir_s, s) = _menhir_stack in
    let _v : (ArithAST.t) =                      ( s ) in
    _menhir_goto_terminated_stmt _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_terminated_stmt : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 74:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | WHILE ->
            Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 75\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 75:\n%!";
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | FLOAT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | INT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | MINUS ->
                Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | NOT ->
                Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | OPENP ->
                Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | STRING _v ->
                Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | TRUE ->
                Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | VAR _v ->
                Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 86:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production terminated_stmt -> FOR OPENP exprC PV expr PV exprC CLOSEP terminated_stmt \n%!";
        let ((((((_menhir_stack, _menhir_s), _, a), _, e), _), _, u), _, t) = _menhir_stack in
        let _v : (ArithAST.t) =                                                                   ( For(a,e,u,t) ) in
        _menhir_goto_terminated_stmt _menhir_env _menhir_stack _menhir_s _v
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 87:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            Printf.fprintf Pervasives.stderr "Shifting (ELSE) to state 88\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 88:\n%!";
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ACOPEN ->
                Printf.fprintf Pervasives.stderr "Shifting (ACOPEN) to state 58\n%!";
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | DO ->
                Printf.fprintf Pervasives.stderr "Shifting (DO) to state 57\n%!";
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | FOR ->
                Printf.fprintf Pervasives.stderr "Shifting (FOR) to state 55\n%!";
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | IF ->
                Printf.fprintf Pervasives.stderr "Shifting (IF) to state 53\n%!";
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | VAR _v ->
                Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 49\n%!";
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | WHILE ->
                Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 1\n%!";
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
        | EOF | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production terminated_stmt -> IF expr terminated_stmt \n%!";
            let (((_menhir_stack, _menhir_s), _, e), _, t) = _menhir_stack in
            let _v : (ArithAST.t) =                               ( If(e,t) ) in
            _menhir_goto_terminated_stmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 89:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production terminated_stmt -> IF expr terminated_stmt ELSE terminated_stmt \n%!";
        let ((((_menhir_stack, _menhir_s), _, e), _, t), _, td) = _menhir_stack in
        let _v : (ArithAST.t) =                                                       ( IfElse(e,t,td) ) in
        _menhir_goto_terminated_stmt _menhir_env _menhir_stack _menhir_s _v
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 90:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production terminated_stmt -> WHILE expr terminated_stmt \n%!";
        let (((_menhir_stack, _menhir_s), _, r), _, t) = _menhir_stack in
        let _v : (ArithAST.t) =                                  ( While(r,t) ) in
        _menhir_goto_terminated_stmt _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 95:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production start2 -> terminated_stmt \n%!";
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (ArithAST.t) =                   ( _1 ) in
        _menhir_goto_start2 _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_start2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    Printf.fprintf Pervasives.stderr "State 96:\n%!";
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        Printf.fprintf Pervasives.stderr "Shifting (EOF) to state 97\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 97:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production start -> start2 EOF \n%!";
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (ArithAST.t) =                   ( _1 ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 98:\n%!";
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

and _menhir_goto_exprC : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState63 | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 62:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            Printf.fprintf Pervasives.stderr "Shifting (PV) to state 63\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 63:\n%!";
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DO ->
                Printf.fprintf Pervasives.stderr "Shifting (DO) to state 57\n%!";
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | VAR _v ->
                Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 49\n%!";
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | ACOFER ->
                _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState48 | MenhirState54 | MenhirState88 | MenhirState85 | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 78:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            Printf.fprintf Pervasives.stderr "Shifting (PV) to state 79\n%!";
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 80:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            Printf.fprintf Pervasives.stderr "Shifting (PV) to state 81\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 81:\n%!";
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | FLOAT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | INT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | MINUS ->
                Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | NOT ->
                Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | OPENP ->
                Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | STRING _v ->
                Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | TRUE ->
                Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | VAR _v ->
                Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 84:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLOSEP ->
            Printf.fprintf Pervasives.stderr "Shifting (CLOSEP) to state 85\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 85:\n%!";
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ACOPEN ->
                Printf.fprintf Pervasives.stderr "Shifting (ACOPEN) to state 58\n%!";
                _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | DO ->
                Printf.fprintf Pervasives.stderr "Shifting (DO) to state 57\n%!";
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | FOR ->
                Printf.fprintf Pervasives.stderr "Shifting (FOR) to state 55\n%!";
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | IF ->
                Printf.fprintf Pervasives.stderr "Shifting (IF) to state 53\n%!";
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | VAR _v ->
                Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 49\n%!";
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | WHILE ->
                Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 1\n%!";
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 99:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            Printf.fprintf Pervasives.stderr "Shifting (PV) to state 79\n%!";
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production start2 -> exprC \n%!";
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (ArithAST.t) =         ( _1 ) in
            _menhir_goto_start2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce2 : _menhir_env -> (('ttv_tail * _menhir_state * (string)) * _menhir_state * (ArithAST.t)) * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "Reducing production assignable -> VAR OPEN expr CLOSE \n%!";
    let (((_menhir_stack, _menhir_s, id), _, t), _) = _menhir_stack in
    let _v : (ArithAST.t) =                           ( Index (id,t) ) in
    _menhir_goto_assignable _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce14 : _menhir_env -> (('ttv_tail * _menhir_state * (string)) * _menhir_state * (ArithAST.t)) * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "Reducing production expr -> VAR OPEN expr CLOSE \n%!";
    let (((_menhir_stack, _menhir_s, id), _, t), _) = _menhir_stack in
    let _v : (ArithAST.t) =                            ( Index (id,t) ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 16:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 18:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 22:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 24:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 26:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 28:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 34:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 30:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run32 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 32:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run36 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 36:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run20 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 20:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_reduce11 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "Reducing production expr -> exprB \n%!";
    let (_menhir_stack, _menhir_s, e) = _menhir_stack in
    let _v : (ArithAST.t) =                   ( e ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 13:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 40:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_goto_stmts_inner : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 59:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        let l = _v in
        Printf.fprintf Pervasives.stderr "Reducing production stmts -> stmts_inner \n%!";
        let _v : (ArithAST.t) =                      ( Stmts l ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 60:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ACOFER ->
            Printf.fprintf Pervasives.stderr "Shifting (ACOFER) to state 61\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 61:\n%!";
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production loop -> ACOPEN stmts ACOFER \n%!";
            let ((_menhir_stack, _menhir_s), _, s) = _menhir_stack in
            let _v : (ArithAST.t) =                             ( s ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 77:\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            let b = _v in
            Printf.fprintf Pervasives.stderr "Reducing production terminated_stmt -> loop \n%!";
            let _v : (ArithAST.t) =                                             ( b ) in
            _menhir_goto_terminated_stmt _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 64:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        let ss = _v in
        Printf.fprintf Pervasives.stderr "Reducing production stmts_inner -> exprC PV stmts_inner \n%!";
        let (_menhir_stack, _menhir_s, s) = _menhir_stack in
        let _v : (ArithAST.t list) =                                                ( s::ss ) in
        _menhir_goto_stmts_inner _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_assignable : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    Printf.fprintf Pervasives.stderr "State 65:\n%!";
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSIGN ->
        Printf.fprintf Pervasives.stderr "Shifting (ASSIGN) to state 72\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 72:\n%!";
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | FLOAT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | INT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NOT ->
            Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | OPENP ->
            Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | TRUE ->
            Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | VAR _v ->
            Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
    | ME ->
        Printf.fprintf Pervasives.stderr "Shifting (ME) to state 70\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 70:\n%!";
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | FLOAT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | INT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | NOT ->
            Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | OPENP ->
            Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | TRUE ->
            Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState70
        | VAR _v ->
            Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | MM ->
        Printf.fprintf Pervasives.stderr "Shifting (MM) to state 69\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 69:\n%!";
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production exprC -> assignable MM \n%!";
        let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _v : (ArithAST.t) =                       ( Assign (id,Bin (Minus, id, Int 1))) in
        _menhir_goto_exprC _menhir_env _menhir_stack _menhir_s _v
    | PE ->
        Printf.fprintf Pervasives.stderr "Shifting (PE) to state 67\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 67:\n%!";
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | FLOAT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | INT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | NOT ->
            Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | OPENP ->
            Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | TRUE ->
            Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | VAR _v ->
            Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | PP ->
        Printf.fprintf Pervasives.stderr "Shifting (PP) to state 66\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 66:\n%!";
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production exprC -> assignable PP \n%!";
        let (_menhir_stack, _menhir_s, id) = _menhir_stack in
        let _v : (ArithAST.t) =                       ( Assign (id,Bin (Plus, id,Int 1))) in
        _menhir_goto_exprC _menhir_env _menhir_stack _menhir_s _v
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
    | MenhirState6 | MenhirState40 | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 15:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | INTE ->
            Printf.fprintf Pervasives.stderr "Shifting (INTE) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 17:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production expr -> expr TIMES expr \n%!";
        let (((_menhir_stack, _menhir_s, l), _), _, r) = _menhir_stack in
        let _v : (ArithAST.t) =                         ( Bin (Times, l, r) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 19:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | ACOPEN | AND | CLOSE | CLOSEP | DEUP | DO | EOF | EQ | FOR | GEQ | GT | IF | INTE | LEQ | LT | MINUS | NEQ | OR | PLUS | PV | VAR _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr PLUS expr \n%!";
            let (((_menhir_stack, _menhir_s, l), _), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                         ( Bin (Plus, l, r) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 21:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production expr -> expr DIVIDE expr \n%!";
        let (((_menhir_stack, _menhir_s, l), _), _, r) = _menhir_stack in
        let _v : (ArithAST.t) =                         ( Bin (Divide, l, r) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 23:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState23
        | ACOPEN | AND | CLOSE | CLOSEP | DEUP | DO | EOF | EQ | FOR | IF | INTE | NEQ | OR | PV | VAR _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprB -> expr NEQ expr \n%!";
            let (((_menhir_stack, _menhir_s, l), _), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                       ( Bin (Different, l, r) ) in
            _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 25:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState25
        | ACOPEN | AND | CLOSE | CLOSEP | DEUP | DO | EOF | EQ | FOR | GEQ | GT | IF | INTE | LEQ | LT | MINUS | NEQ | OR | PLUS | PV | VAR _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr MINUS expr \n%!";
            let (((_menhir_stack, _menhir_s, l), _), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                         ( Bin (Minus, l, r) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 27:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | ACOPEN | AND | CLOSE | CLOSEP | DEUP | DO | EOF | EQ | FOR | GEQ | GT | IF | INTE | LEQ | LT | NEQ | OR | PV | VAR _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprB -> expr LT expr \n%!";
            let (((_menhir_stack, _menhir_s, l), _), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                      ( Bin (LessThan, l, r) ) in
            _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 29:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | ACOPEN | AND | CLOSE | CLOSEP | DEUP | DO | EOF | EQ | FOR | GEQ | GT | IF | INTE | LEQ | LT | NEQ | OR | PV | VAR _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprB -> expr LEQ expr \n%!";
            let (((_menhir_stack, _menhir_s, l), _), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                        ( Bin (LessThanEq, l, r) ) in
            _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 31:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | ACOPEN | AND | CLOSE | CLOSEP | DEUP | DO | EOF | EQ | FOR | GEQ | GT | IF | INTE | LEQ | LT | NEQ | OR | PV | VAR _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprB -> expr GT expr \n%!";
            let (((_menhir_stack, _menhir_s, l), _), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                     ( Bin (GreaterThan, l, r) ) in
            _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 33:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState33
        | ACOPEN | AND | CLOSE | CLOSEP | DEUP | DO | EOF | EQ | FOR | GEQ | GT | IF | INTE | LEQ | LT | NEQ | OR | PV | VAR _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprB -> expr GEQ expr \n%!";
            let (((_menhir_stack, _menhir_s, l), _), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                       ( Bin (GreaterThanEq, l, r) ) in
            _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 35:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEUP ->
            Printf.fprintf Pervasives.stderr "Shifting (DEUP) to state 38\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState35 in
            Printf.fprintf Pervasives.stderr "State 38:\n%!";
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | FLOAT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | INT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | MINUS ->
                Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | NOT ->
                Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | OPENP ->
                Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | STRING _v ->
                Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | TRUE ->
                Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | VAR _v ->
                Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | INTE ->
            Printf.fprintf Pervasives.stderr "Shifting (INTE) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 37:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState37
        | ACOPEN | AND | CLOSE | CLOSEP | DEUP | DO | EOF | EQ | FOR | IF | INTE | NEQ | OR | PV | VAR _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprB -> expr EQ expr \n%!";
            let (((_menhir_stack, _menhir_s, l), _), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                       ( Bin (Equal, l, r) ) in
            _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 39:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | INTE ->
            Printf.fprintf Pervasives.stderr "Shifting (INTE) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | ACOPEN | AND | CLOSE | CLOSEP | DEUP | DO | EOF | FOR | IF | OR | PV | VAR _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr INTE expr DEUP expr \n%!";
            let (((((_menhir_stack, _menhir_s, t), _), _, e), _), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                                     ( Tern(t,e,r)) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 42:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production expr -> MINUS expr \n%!";
        let ((_menhir_stack, _menhir_s), _, t) = _menhir_stack in
        let _v : (ArithAST.t) =                         ( Un (UMinus,t) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 43:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production expr -> NOT expr \n%!";
        let ((_menhir_stack, _menhir_s), _, t) = _menhir_stack in
        let _v : (ArithAST.t) =                         ( Un (Not,t) ) in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 46:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLOSE ->
            Printf.fprintf Pervasives.stderr "Shifting (CLOSE) to state 47\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState46 in
            Printf.fprintf Pervasives.stderr "State 47:\n%!";
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | INTE ->
            Printf.fprintf Pervasives.stderr "Shifting (INTE) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState46
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 48:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ACOPEN ->
            Printf.fprintf Pervasives.stderr "Shifting (ACOPEN) to state 58\n%!";
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | DO ->
            Printf.fprintf Pervasives.stderr "Shifting (DO) to state 57\n%!";
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | FOR ->
            Printf.fprintf Pervasives.stderr "Shifting (FOR) to state 55\n%!";
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | IF ->
            Printf.fprintf Pervasives.stderr "Shifting (IF) to state 53\n%!";
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | INTE ->
            Printf.fprintf Pervasives.stderr "Shifting (INTE) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | VAR _v ->
            Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 49\n%!";
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
        | WHILE ->
            Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 1\n%!";
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 51:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLOSE ->
            Printf.fprintf Pervasives.stderr "Shifting (CLOSE) to state 52\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState51 in
            Printf.fprintf Pervasives.stderr "State 52:\n%!";
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _ = _menhir_discard _menhir_env in
            _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | INTE ->
            Printf.fprintf Pervasives.stderr "Shifting (INTE) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 54:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ACOPEN ->
            Printf.fprintf Pervasives.stderr "Shifting (ACOPEN) to state 58\n%!";
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | DO ->
            Printf.fprintf Pervasives.stderr "Shifting (DO) to state 57\n%!";
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | FOR ->
            Printf.fprintf Pervasives.stderr "Shifting (FOR) to state 55\n%!";
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | IF ->
            Printf.fprintf Pervasives.stderr "Shifting (IF) to state 53\n%!";
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | INTE ->
            Printf.fprintf Pervasives.stderr "Shifting (INTE) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | VAR _v ->
            Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 49\n%!";
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | WHILE ->
            Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 1\n%!";
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 68:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | INTE ->
            Printf.fprintf Pervasives.stderr "Shifting (INTE) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | CLOSEP | EOF | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprC -> assignable PE expr \n%!";
            let ((_menhir_stack, _menhir_s, id), _, t) = _menhir_stack in
            let _v : (ArithAST.t) =                            ( Assign (id,Bin (Plus, id, t))) in
            _menhir_goto_exprC _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 71:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | INTE ->
            Printf.fprintf Pervasives.stderr "Shifting (INTE) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | CLOSEP | EOF | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprC -> assignable ME expr \n%!";
            let ((_menhir_stack, _menhir_s, id), _, t) = _menhir_stack in
            let _v : (ArithAST.t) =                             ( Assign (id,Bin (Minus, id,  t))) in
            _menhir_goto_exprC _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 73:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | INTE ->
            Printf.fprintf Pervasives.stderr "Shifting (INTE) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | CLOSEP | EOF | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprC -> assignable ASSIGN expr \n%!";
            let ((_menhir_stack, _menhir_s, id), _, t) = _menhir_stack in
            let _v : (ArithAST.t) =                                 (  Assign (id,t)) in
            _menhir_goto_exprC _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 76:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | INTE ->
            Printf.fprintf Pervasives.stderr "Shifting (INTE) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | CLOSEP | EOF | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprC -> DO terminated_stmt WHILE expr \n%!";
            let (((_menhir_stack, _menhir_s), _, t), _, e) = _menhir_stack in
            let _v : (ArithAST.t) =                                     ( Do(t,e) ) in
            _menhir_goto_exprC _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 82:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | INTE ->
            Printf.fprintf Pervasives.stderr "Shifting (INTE) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PV ->
            Printf.fprintf Pervasives.stderr "Shifting (PV) to state 83\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState82 in
            Printf.fprintf Pervasives.stderr "State 83:\n%!";
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DO ->
                Printf.fprintf Pervasives.stderr "Shifting (DO) to state 57\n%!";
                _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | VAR _v ->
                Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 49\n%!";
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 93:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CLOSE ->
            Printf.fprintf Pervasives.stderr "Shifting (CLOSE) to state 94\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState93 in
            Printf.fprintf Pervasives.stderr "State 94:\n%!";
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | DIVIDE | EOF | EQ | GEQ | GT | INTE | LEQ | LT | MINUS | NEQ | PLUS | TIMES ->
                _menhir_reduce14 _menhir_env (Obj.magic _menhir_stack)
            | ASSIGN | ME | MM | PE | PP ->
                _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | INTE ->
            Printf.fprintf Pervasives.stderr "Shifting (INTE) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 100:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 36\n%!";
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 32\n%!";
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | INTE ->
            Printf.fprintf Pervasives.stderr "Shifting (INTE) to state 34\n%!";
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production start2 -> expr \n%!";
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (ArithAST.t) =        ( _1 ) in
            _menhir_goto_start2 _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | _ ->
        _menhir_fail ()

and _menhir_goto_exprB : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState92 | MenhirState81 | MenhirState75 | MenhirState72 | MenhirState70 | MenhirState67 | MenhirState53 | MenhirState50 | MenhirState1 | MenhirState3 | MenhirState7 | MenhirState38 | MenhirState36 | MenhirState34 | MenhirState32 | MenhirState30 | MenhirState28 | MenhirState26 | MenhirState24 | MenhirState22 | MenhirState20 | MenhirState18 | MenhirState16 | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 12:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            Printf.fprintf Pervasives.stderr "Shifting (AND) to state 40\n%!";
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            Printf.fprintf Pervasives.stderr "Shifting (OR) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | ACOPEN | CLOSE | CLOSEP | DEUP | DIVIDE | DO | EOF | EQ | FOR | GEQ | GT | IF | INTE | LEQ | LT | MINUS | NEQ | PLUS | PV | TIMES | VAR _ | WHILE ->
            _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 14:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ACOPEN | AND | CLOSE | CLOSEP | DEUP | DO | EOF | FOR | IF | OR | PV | VAR _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprB -> exprB OR exprB \n%!";
            let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                       ( Bin (Or, l, r) ) in
            _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
        | DIVIDE | EQ | GEQ | GT | INTE | LEQ | LT | MINUS | NEQ | PLUS | TIMES ->
            _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
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
        | ACOPEN | AND | CLOSE | CLOSEP | DEUP | DO | EOF | FOR | IF | OR | PV | VAR _ | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprB -> exprB AND exprB \n%!";
            let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                         ( Bin (And, l, r) ) in
            _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
        | DIVIDE | EQ | GEQ | GT | INTE | LEQ | LT | MINUS | NEQ | PLUS | TIMES ->
            _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 44:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            Printf.fprintf Pervasives.stderr "Shifting (AND) to state 40\n%!";
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
        | CLOSEP ->
            Printf.fprintf Pervasives.stderr "Shifting (CLOSEP) to state 45\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 45:\n%!";
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production exprB -> OPENP exprB CLOSEP \n%!";
            let ((_menhir_stack, _menhir_s), _, b) = _menhir_stack in
            let _v : (ArithAST.t) =                         (b) in
            _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v
        | OR ->
            Printf.fprintf Pervasives.stderr "Shifting (OR) to state 13\n%!";
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE | EQ | GEQ | GT | INTE | LEQ | LT | MINUS | NEQ | PLUS | TIMES ->
            _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "Reducing production stmts_inner -> \n%!";
    let _v : (ArithAST.t list) =             ( [] ) in
    _menhir_goto_stmts_inner _menhir_env _menhir_stack _menhir_s _v

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 49:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | OPEN ->
        Printf.fprintf Pervasives.stderr "Shifting (OPEN) to state 50\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 50:\n%!";
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | FLOAT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | INT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | NOT ->
            Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | OPENP ->
            Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | TRUE ->
            Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | VAR _v ->
            Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
    | ASSIGN | ME | MM | PE | PP ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 1:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_reduce1 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "Reducing production assignable -> VAR \n%!";
    let (_menhir_stack, _menhir_s, v) = _menhir_stack in
    let _v : (ArithAST.t) =        ( Var v ) in
    _menhir_goto_assignable _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce6 : _menhir_env -> 'ttv_tail * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "Reducing production expr -> VAR \n%!";
    let (_menhir_stack, _menhir_s, s) = _menhir_stack in
    let _v : (ArithAST.t) =                         ( Var s ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 2:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | OPEN ->
        Printf.fprintf Pervasives.stderr "Shifting (OPEN) to state 3\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 3:\n%!";
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | FALSE ->
            Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | FLOAT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | INT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | NOT ->
            Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | OPENP ->
            Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | STRING _v ->
            Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | TRUE ->
            Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | VAR _v ->
            Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
    | ACOPEN | AND | CLOSE | CLOSEP | DEUP | DIVIDE | DO | EOF | EQ | FOR | GEQ | GT | IF | INTE | LEQ | LT | MINUS | NEQ | OR | PLUS | PV | TIMES | VAR _ | WHILE ->
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 4:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    Printf.fprintf Pervasives.stderr "Reducing production exprB -> TRUE \n%!";
    let _v : (ArithAST.t) =          (True) in
    _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 5:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let s = _v in
    Printf.fprintf Pervasives.stderr "Reducing production expr -> STRING \n%!";
    let _v : (ArithAST.t) =                         ( String s ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 6:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 7:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 8:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 9:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    Printf.fprintf Pervasives.stderr "Reducing production expr -> INT \n%!";
    let _v : (ArithAST.t) =                         ( Int i ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 53:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 55:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 56\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 56:\n%!";
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | DO ->
            Printf.fprintf Pervasives.stderr "Shifting (DO) to state 57\n%!";
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState56
        | VAR _v ->
            Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 49\n%!";
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 10:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let f = _v in
    Printf.fprintf Pervasives.stderr "Reducing production expr -> FLOAT \n%!";
    let _v : (ArithAST.t) =                         ( Float f ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 11:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    Printf.fprintf Pervasives.stderr "Reducing production exprB -> FALSE \n%!";
    let _v : (ArithAST.t) =           (False) in
    _menhir_goto_exprB _menhir_env _menhir_stack _menhir_s _v

and _menhir_run57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 57:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ACOPEN ->
        Printf.fprintf Pervasives.stderr "Shifting (ACOPEN) to state 58\n%!";
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | DO ->
        Printf.fprintf Pervasives.stderr "Shifting (DO) to state 57\n%!";
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FOR ->
        Printf.fprintf Pervasives.stderr "Shifting (FOR) to state 55\n%!";
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IF ->
        Printf.fprintf Pervasives.stderr "Shifting (IF) to state 53\n%!";
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 49\n%!";
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | WHILE ->
        Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 58:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DO ->
        Printf.fprintf Pervasives.stderr "Shifting (DO) to state 57\n%!";
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 49\n%!";
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | ACOFER ->
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ACOFER ->
        "ACOFER"
    | ACOPEN ->
        "ACOPEN"
    | AND ->
        "AND"
    | ASSIGN ->
        "ASSIGN"
    | CLOSE ->
        "CLOSE"
    | CLOSEP ->
        "CLOSEP"
    | DEUP ->
        "DEUP"
    | DIVIDE ->
        "DIVIDE"
    | DO ->
        "DO"
    | ELSE ->
        "ELSE"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | FALSE ->
        "FALSE"
    | FLOAT _ ->
        "FLOAT"
    | FOR ->
        "FOR"
    | GEQ ->
        "GEQ"
    | GT ->
        "GT"
    | IF ->
        "IF"
    | INT _ ->
        "INT"
    | INTE ->
        "INTE"
    | LEQ ->
        "LEQ"
    | LT ->
        "LT"
    | ME ->
        "ME"
    | MINUS ->
        "MINUS"
    | MM ->
        "MM"
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
    | PE ->
        "PE"
    | PLUS ->
        "PLUS"
    | PP ->
        "PP"
    | PV ->
        "PV"
    | STRING _ ->
        "STRING"
    | TIMES ->
        "TIMES"
    | TRUE ->
        "TRUE"
    | VAR _ ->
        "VAR"
    | WHILE ->
        "WHILE"

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
    | ACOPEN ->
        Printf.fprintf Pervasives.stderr "Shifting (ACOPEN) to state 58\n%!";
        _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DO ->
        Printf.fprintf Pervasives.stderr "Shifting (DO) to state 57\n%!";
        _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FALSE ->
        Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | FOR ->
        Printf.fprintf Pervasives.stderr "Shifting (FOR) to state 55\n%!";
        _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IF ->
        Printf.fprintf Pervasives.stderr "Shifting (IF) to state 53\n%!";
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | OPENP ->
        Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | TRUE ->
        Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 91\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState0 in
        Printf.fprintf Pervasives.stderr "State 91:\n%!";
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | OPEN ->
            Printf.fprintf Pervasives.stderr "Shifting (OPEN) to state 92\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 92:\n%!";
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | FALSE ->
                Printf.fprintf Pervasives.stderr "Shifting (FALSE) to state 11\n%!";
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | FLOAT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 10\n%!";
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | INT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (INT) to state 9\n%!";
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | MINUS ->
                Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 8\n%!";
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | NOT ->
                Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 7\n%!";
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | OPENP ->
                Printf.fprintf Pervasives.stderr "Shifting (OPENP) to state 6\n%!";
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | STRING _v ->
                Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 5\n%!";
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | TRUE ->
                Printf.fprintf Pervasives.stderr "Shifting (TRUE) to state 4\n%!";
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState92
            | VAR _v ->
                Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 2\n%!";
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92)
        | DIVIDE | EOF | EQ | GEQ | GT | INTE | LEQ | LT | MINUS | NEQ | PLUS | TIMES ->
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | ME | MM | PE | PP ->
            _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | WHILE ->
        Printf.fprintf Pervasives.stderr "Shifting (WHILE) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



