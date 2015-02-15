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

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState58
  | MenhirState55
  | MenhirState53
  | MenhirState51
  | MenhirState48
  | MenhirState45
  | MenhirState43
  | MenhirState39
  | MenhirState30
  | MenhirState28
  | MenhirState26
  | MenhirState24
  | MenhirState22
  | MenhirState20
  | MenhirState18
  | MenhirState16
  | MenhirState14
  | MenhirState12
  | MenhirState10
  | MenhirState8
  | MenhirState4
  | MenhirState3
  | MenhirState0

  
 
 
(** parser *)

open ArithAST

let _eRR =
  Error

let rec _menhir_goto_manual_arith_plus_l : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    Printf.fprintf Pervasives.stderr "State 57:\n%!";
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EOF ->
        Printf.fprintf Pervasives.stderr "Shifting (EOF) to state 61\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 61:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production start1 -> int_plus_left int_plus_right manual_arith_plus_r manual_arith_plus_l EOF \n%!";
        let ((((_menhir_stack, _1), _, _2), _, _3), _, _4) = _menhir_stack in
        let _v : (ArithAST.t) = ( Dummy ("assocs and precs", [_1;_2;_3;_4]) ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 38:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        Printf.fprintf Pervasives.stderr "Accepting\n%!";
        Obj.magic _1
    | PLUS ->
        Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 58\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 58:\n%!";
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | INT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INT) to state 46\n%!";
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 55:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 46\n%!";
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55

and _menhir_goto_manual_arith_plus_r : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 49:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production manual_arith_plus_r -> manual_arith_times_r PLUS manual_arith_plus_r \n%!";
        let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
        let _v : (ArithAST.t) =                                                     ( Bin (Plus, l,r) ) in
        _menhir_goto_manual_arith_plus_r _menhir_env _menhir_stack _menhir_s _v
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 53:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INT) to state 46\n%!";
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | _ ->
        _menhir_fail ()

and _menhir_goto_manual_arith_times_l : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 54:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 55\n%!";
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | EOF | PLUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production manual_arith_plus_l -> manual_arith_times_l \n%!";
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (ArithAST.t) =                                                     ( _1 ) in
            _menhir_goto_manual_arith_plus_l _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 59:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 55\n%!";
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack)
        | EOF | PLUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production manual_arith_plus_l -> manual_arith_plus_l PLUS manual_arith_times_l \n%!";
            let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                                                     ( Bin (Plus, l,r) ) in
            _menhir_goto_manual_arith_plus_l _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_manual_arith_times_r : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState48 | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 47:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 48\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 48:\n%!";
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | INT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (INT) to state 46\n%!";
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
        | INT _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production manual_arith_plus_r -> manual_arith_times_r \n%!";
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (ArithAST.t) =                                                     ( _1 ) in
            _menhir_goto_manual_arith_plus_r _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 52:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production manual_arith_times_r -> manual_arith_atom TIMES manual_arith_times_r \n%!";
        let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
        let _v : (ArithAST.t) =                                                     ( Bin (Times, l,r) ) in
        _menhir_goto_manual_arith_times_r _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 46:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    Printf.fprintf Pervasives.stderr "Reducing production manual_arith_atom -> INT \n%!";
    let _v : (ArithAST.t) =                                                     ( Int _1 ) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState45 | MenhirState51 | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 50:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 51\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 51:\n%!";
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | INT _v ->
                Printf.fprintf Pervasives.stderr "Shifting (INT) to state 46\n%!";
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
        | INT _ | PLUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production manual_arith_times_r -> manual_arith_atom \n%!";
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (ArithAST.t) =                                                     ( _1 ) in
            _menhir_goto_manual_arith_times_r _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 56:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production manual_arith_times_l -> manual_arith_times_l TIMES manual_arith_atom \n%!";
        let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
        let _v : (ArithAST.t) =                                                     ( Bin (Times, l,r) ) in
        _menhir_goto_manual_arith_times_l _menhir_env _menhir_stack _menhir_s _v
    | MenhirState53 | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 60:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production manual_arith_times_l -> manual_arith_atom \n%!";
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (ArithAST.t) =                                                     ( _1 ) in
        _menhir_goto_manual_arith_times_l _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run12 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 12:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 14:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
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
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
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
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 22:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 24:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 26:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run28 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 28:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run18 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 18:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 30:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run8 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 8:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run10 : _menhir_env -> 'ttv_tail * _menhir_state * (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    Printf.fprintf Pervasives.stderr "State 10:\n%!";
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_goto_int_plus_right : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 44:\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production int_plus_right -> INT PLUS int_plus_right \n%!";
        let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
        let _v : (ArithAST.t) =                               ( Bin (Plus, Int l, r) ) in
        _menhir_goto_int_plus_right _menhir_env _menhir_stack _menhir_s _v
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 45:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | INT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INT) to state 46\n%!";
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 7:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | AND | DIVIDE | EOF | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> MINUS expr \n%!";
            let ((_menhir_stack, _menhir_s), _, t) = _menhir_stack in
            let _v : (ArithAST.t) =                         ( Un (UMinus,t) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 9:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            Printf.fprintf Pervasives.stderr "Shifting (AND) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            Printf.fprintf Pervasives.stderr "Shifting (OR) to state 12\n%!";
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr TIMES expr \n%!";
            let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                         ( Bin (Times, l, r) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 11:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            Printf.fprintf Pervasives.stderr "Shifting (AND) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            Printf.fprintf Pervasives.stderr "Shifting (OR) to state 12\n%!";
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
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
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 13:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr OR expr \n%!";
            let ((_menhir_stack, _menhir_s, l0), _, r0) = _menhir_stack in
            let _v : (ArithAST.t) = let e =
              let r = r0 in
              let l = l0 in
              let o =
                        ( Or )
              in
                                                      ( Bin (o, l, r) )
            in
                                    ( e ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | EQ | NEQ | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr NEQ expr \n%!";
            let ((_menhir_stack, _menhir_s, l0), _, r0) = _menhir_stack in
            let _v : (ArithAST.t) = let e =
              let r = r0 in
              let l = l0 in
              let o =
                        ( Different )
              in
                                                      ( Bin (o, l, r) )
            in
                                    ( e ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 17:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | OR ->
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
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 19:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | AND | DIVIDE | EOF | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr DIVIDE expr \n%!";
            let ((_menhir_stack, _menhir_s, l), _, r) = _menhir_stack in
            let _v : (ArithAST.t) =                         ( Bin (Divide, l, r) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 21:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | EQ | GEQ | GT | LEQ | LT | NEQ | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr LT expr \n%!";
            let ((_menhir_stack, _menhir_s, l0), _, r0) = _menhir_stack in
            let _v : (ArithAST.t) = let e =
              let r = r0 in
              let l = l0 in
              let o =
                        ( LessThan )
              in
                                                      ( Bin (o, l, r) )
            in
                                    ( e ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 23:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | EQ | GEQ | GT | LEQ | LT | NEQ | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr LEQ expr \n%!";
            let ((_menhir_stack, _menhir_s, l0), _, r0) = _menhir_stack in
            let _v : (ArithAST.t) = let e =
              let r = r0 in
              let l = l0 in
              let o =
                        ( LessThanEq )
              in
                                                      ( Bin (o, l, r) )
            in
                                    ( e ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 25:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | EQ | GEQ | GT | LEQ | LT | NEQ | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr GT expr \n%!";
            let ((_menhir_stack, _menhir_s, l0), _, r0) = _menhir_stack in
            let _v : (ArithAST.t) = let e =
              let r = r0 in
              let l = l0 in
              let o =
                        ( GreaterThan )
              in
                                                      ( Bin (o, l, r) )
            in
                                    ( e ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 27:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | EQ | GEQ | GT | LEQ | LT | NEQ | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr GEQ expr \n%!";
            let ((_menhir_stack, _menhir_s, l0), _, r0) = _menhir_stack in
            let _v : (ArithAST.t) = let e =
              let r = r0 in
              let l = l0 in
              let o =
                        ( GreaterThanEq )
              in
                                                      ( Bin (o, l, r) )
            in
                                    ( e ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 29:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | EQ | NEQ | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr EQ expr \n%!";
            let ((_menhir_stack, _menhir_s, l0), _, r0) = _menhir_stack in
            let _v : (ArithAST.t) = let e =
              let r = r0 in
              let l = l0 in
              let o =
                        ( Equal )
              in
                                                      ( Bin (o, l, r) )
            in
                                    ( e ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 31:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | AND | EOF | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> expr AND expr \n%!";
            let ((_menhir_stack, _menhir_s, l0), _, r0) = _menhir_stack in
            let _v : (ArithAST.t) = let e =
              let r = r0 in
              let l = l0 in
              let o =
                        ( And )
              in
                                                      ( Bin (o, l, r) )
            in
                                    ( e ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 32:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | AND | DIVIDE | EOF | EQ | GEQ | GT | LEQ | LT | MINUS | NEQ | OR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production expr -> NOT expr \n%!";
            let ((_menhir_stack, _menhir_s), _, t) = _menhir_stack in
            let _v : (ArithAST.t) =                         ( Un (Not,t) ) in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 34:\n%!";
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            Printf.fprintf Pervasives.stderr "Shifting (AND) to state 30\n%!";
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack)
        | DIVIDE ->
            Printf.fprintf Pervasives.stderr "Shifting (DIVIDE) to state 18\n%!";
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            Printf.fprintf Pervasives.stderr "Shifting (EOF) to state 35\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 35:\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "Reducing production start -> expr EOF \n%!";
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (ArithAST.t) =                 ( _1 ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 33:\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Printf.fprintf Pervasives.stderr "Accepting\n%!";
            Obj.magic _1
        | EQ ->
            Printf.fprintf Pervasives.stderr "Shifting (EQ) to state 28\n%!";
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (GEQ) to state 26\n%!";
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            Printf.fprintf Pervasives.stderr "Shifting (GT) to state 24\n%!";
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (LEQ) to state 22\n%!";
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            Printf.fprintf Pervasives.stderr "Shifting (LT) to state 20\n%!";
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 16\n%!";
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            Printf.fprintf Pervasives.stderr "Shifting (NEQ) to state 14\n%!";
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            Printf.fprintf Pervasives.stderr "Shifting (OR) to state 12\n%!";
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 10\n%!";
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            Printf.fprintf Pervasives.stderr "Shifting (TIMES) to state 8\n%!";
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 42:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | PLUS ->
        Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 43\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 43:\n%!";
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | INT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INT) to state 42\n%!";
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | INT _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "Reducing production int_plus_right -> INT \n%!";
        let (_menhir_stack, _menhir_s, i) = _menhir_stack in
        let _v : (ArithAST.t) =                               ( Int i ) in
        _menhir_goto_int_plus_right _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | AND ->
        "AND"
    | DIVIDE ->
        "DIVIDE"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
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
    | OR ->
        "OR"
    | PLUS ->
        "PLUS"
    | STRING _ ->
        "STRING"
    | TIMES ->
        "TIMES"
    | VAR _ ->
        "VAR"

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
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
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 1:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let s = _v in
    Printf.fprintf Pervasives.stderr "Reducing production expr -> VAR \n%!";
    let _v : (ArithAST.t) =                         ( Var s ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 2:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let s = _v in
    Printf.fprintf Pervasives.stderr "Reducing production expr -> STRING \n%!";
    let _v : (ArithAST.t) =                         ( String s ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 3:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    Printf.fprintf Pervasives.stderr "State 4:\n%!";
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 5:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    Printf.fprintf Pervasives.stderr "Reducing production expr -> INT \n%!";
    let _v : (ArithAST.t) =                         ( Int i ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (float) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    Printf.fprintf Pervasives.stderr "State 6:\n%!";
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let f = _v in
    Printf.fprintf Pervasives.stderr "Reducing production expr -> FLOAT \n%!";
    let _v : (ArithAST.t) =                         ( Float f ) in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_int_plus_left : _menhir_env -> 'ttv_tail -> (ArithAST.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    Printf.fprintf Pervasives.stderr "State 39:\n%!";
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 42\n%!";
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | PLUS ->
        Printf.fprintf Pervasives.stderr "Shifting (PLUS) to state 40\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState39 in
        Printf.fprintf Pervasives.stderr "State 40:\n%!";
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | INT _v ->
            Printf.fprintf Pervasives.stderr "Shifting (INT) to state 41\n%!";
            let _menhir_stack = Obj.magic _menhir_stack in
            Printf.fprintf Pervasives.stderr "State 41:\n%!";
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let r = _v in
            Printf.fprintf Pervasives.stderr "Reducing production int_plus_left -> int_plus_left PLUS INT \n%!";
            let ((_menhir_stack, l), _) = _menhir_stack in
            let _v : (ArithAST.t) =                               ( Bin (Plus, l, Int r) ) in
            _menhir_goto_int_plus_left _menhir_env _menhir_stack _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

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

and _menhir_init : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> _menhir_env =
  fun lexer lexbuf ->
    let _tok = lexer lexbuf in
    Printf.fprintf Pervasives.stderr "Lookahead token is now %s (%d-%d)\n%!" (_menhir_print_token _tok) lexbuf.Lexing.lex_start_p.Lexing.pos_cnum lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum;
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = 4611686018427387903;
      }

and start : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ArithAST.t) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = () in
    Printf.fprintf Pervasives.stderr "State 0:\n%!";
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (FLOAT) to state 6\n%!";
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 5\n%!";
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | MINUS ->
        Printf.fprintf Pervasives.stderr "Shifting (MINUS) to state 4\n%!";
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NOT ->
        Printf.fprintf Pervasives.stderr "Shifting (NOT) to state 3\n%!";
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRING _v ->
        Printf.fprintf Pervasives.stderr "Shifting (STRING) to state 2\n%!";
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | VAR _v ->
        Printf.fprintf Pervasives.stderr "Shifting (VAR) to state 1\n%!";
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

and start1 : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (ArithAST.t) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = () in
    Printf.fprintf Pervasives.stderr "State 36:\n%!";
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        Printf.fprintf Pervasives.stderr "Shifting (INT) to state 37\n%!";
        let _menhir_stack = Obj.magic _menhir_stack in
        Printf.fprintf Pervasives.stderr "State 37:\n%!";
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let i = _v in
        Printf.fprintf Pervasives.stderr "Reducing production int_plus_left -> INT \n%!";
        let _v : (ArithAST.t) =                               ( Int i ) in
        _menhir_goto_int_plus_left _menhir_env _menhir_stack _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        Printf.fprintf Pervasives.stderr "Initiating error handling\n%!";
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR)



