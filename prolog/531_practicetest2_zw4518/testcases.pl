:- use_module(library(plunit)).


% ---------------------------------------------------------------------------- %
% Unit tests for the algebraic expressions question.
% ---------------------------------------------------------------------------- %


% ------------------------------ eval ---------------------------------------- %

:- begin_tests(eval).

test(const, true(V =:= 3.5)) :-
  eval(3.5, [(x,2.0)], V).

test(variable, true(V =:= 2.0)) :-
  eval(x, [(x,2.0)], V).

test(neg, true(V =:= -2.0)) :-
  eval(-x, [(x,2.0)], V).

test(plus, true(V =:= 8.0)) :-
  eval(x+3.0, [(x,5.0)], V).

test(times, true(V =:= 10.0)) :-
  eval(x*5.0, [(x,2.0)], V).

test(sin, true(V =:= sin(2.0))) :-
  eval(sin(x), [(x,2.0)], V).

test(cos, true(V =:= cos(2.0))) :-
  eval(cos(x), [(x,2.0)], V).

:- end_tests(eval).


% ------------------------------ commutes ------------------------------------ %

:- begin_tests(commutes).

test(num) :-
  commutes(1.0, 1.0).

test(variable) :-
  commutes(x, x).

test(neg) :-
  commutes(-x, -x).

test(plus) :-
  commutes(x+1.0, 1.0+x).

test(plus_identical) :-
  commutes(x+1.0, x+1.0).

test(times) :-
  commutes(x*y, y*x).

test(sin) :-
  commutes(sin(x), sin(x)).

test(sin_of_mul) :-
  commutes(sin(2.0*y), sin(y*2.0)).

test(cos) :-
  commutes(cos(x), cos(x)).

test(same_val_only, fail) :-
  commutes(3.0+1.0, 2.0+2.0).

:- end_tests(commutes).


% ------------------------------ diff ---------------------------------------- %

:- begin_tests(diff).

test(num, true(D =:= 0.0)) :-
  diff(2.5, x, D).

test(variable, true(D =:= 1.0)) :-
  diff(x, x, D).

test(different_variable, true(D =:= 0.0)) :-
  diff(y, x, D).

test(negation, true(D =:= -1.0)) :-
  diff(-x, x, D).

test(sum_of_var, true(commutes(D, 1.0+1.0))) :-
  diff(x+x, x, D).

test(prod_of_const_and_var, true(D =:= 2.0)) :-
  diff(2.0*x, x, D).

test(prod_of_var, true(D == x+x)) :-
  diff(x*x, x, D).

test(sin, true(D == cos(x))) :-
  diff(sin(x), x, D).

test(cos, true(D == -sin(x))) :-
  diff(cos(x), x, D).

:- end_tests(diff).


% ------------------------------ maclaurin ----------------------------------- %

:- begin_tests(maclaurin).

test(constant, [true(V =:= 4)]) :-
  maclaurin(4.0, 2.0, 5, V).

test(variable_n1, [true(V =:= 0)]) :-
  maclaurin(x, 2.0, 1, V).

test(variable_n2, [true(V =:= 2)]) :-
  maclaurin(x, 2.0, 2, V).

test(neg, [true(V =:= -2)]) :-
  maclaurin(-x, 2.0, 2, V).

test(plus, [true(V =:= 5)]) :-
  maclaurin(x+3.0, 2.0, 2, V).

test(times, [true(V =:= 8)]) :-
  maclaurin(x*4.0, 2.0, 2, V).

test(sin2n1, [true(V =:= 0)]) :-
  maclaurin(sin(x), 2.0, 1, V).

test(sin2n2, [true(V =:= sin(0)+cos(0)*2)]) :-
  maclaurin(sin(x), 2.0, 2, V).

test(cos5n3, [true(V =:= cos(0)-3*sin(0)-9*cos(0)/2)]) :-
  maclaurin(cos(x), 3.0, 3, V).

:- end_tests(maclaurin).



% ---------------------------------------------------------------------------- %
% Unit tests for the tiles puzzle.
% ---------------------------------------------------------------------------- %


% ------------------------------ b_to_left ----------------------------------- %

:- begin_tests(b_to_left).

test('starting position', true(H == 9)) :-
  b_to_left([b,b,b,e,w,w,w], H).

test('one switch', true(H == 8)) :-
  b_to_left([b,b,e,w,b,w,w], H).

test('all switched', true(H == 0)) :-
  b_to_left([w,w,e,w,b,b,b], H).

:- end_tests(b_to_left).


% ------------------------------ move ---------------------------------------- %

:- begin_tests(move).

test('initial move',
  set(Pos == 
    [
      [b,b,b,w,e,w,w],
      [b,b,b,w,w,e,w],
      [b,b,b,w,w,w,e],
      [b,b,e,b,w,w,w],
      [b,e,b,b,w,w,w],
      [e,b,b,b,w,w,w]
    ])) :-
  move([b,b,b,e,w,w,w], Pos).

test('space on left',
  set(Pos == 
    [
      [b,b,b,e,w,w,w],
      [b,b,e,b,w,w,w],
      [b,e,b,b,w,w,w]
    ])) :-
  move([e,b,b,b,w,w,w], Pos).

:- end_tests(move).


% ------------------------------ search_agenda ------------------------------- %

:- begin_tests(search_agenda).

test(
  'start from goal',
  true(
    (
      Visited,
      Final
    ) =
    (
      [],
      n(3, 3, [w,w,e,w,b,b,b], _, 5)
    )
  )) :-
    search_agenda([n(3, 3, [w,w,e,w,b,b,b], _, 5)], Visited, Final).

test(
  'one step required',
  true(
    (
      Visited,
      Final
    ) =
    (
      [n(4, 3, [w,w,b,w,e,b,b],0,5)],
      n(4, 4, [w,w,e,w,b,b,b],_,0)
    )
  )) :-
    search_agenda([n(4, 3, [w,w,b,w,e,b,b], _, 5)], Visited, Final).

:- end_tests(search_agenda).



% ------------------------------ trace_moves --------------------------------- %

:- begin_tests(trace_moves).

test('no moves', true(Seq == [[b,b,b,e,w,w,w]-9])) :-
  trace_moves(n(9, 0, [b,b,b,e,w,w,w], _, none), [], Seq).

test('one move', true(Seq == [[b,b,b,e,w,w,w]-9,[b,b,e,b,w,w,w]-9])) :-
  trace_moves(
    n(10, 1, [b,b,e,b,w,w,w], _, 0),
    [n(9, 0, [b,b,b,e,w,w,w], 0, none)],
    Seq
  ).

:- end_tests(trace_moves).

