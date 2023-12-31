% To run these tests, enter `swipl util.plt`

% Suppress warnings about singleton variables.
:-style_check(-singleton).

:- consult(util).

:- begin_tests(util).

test(column) :-
  Board = [
    [' ', ' ', a,   ' ', ' ', ' '],
    [' ', ' ', b,   ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', x,   ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ']
  ],
  column(2, Board, Column),
  assertion(Column == [a, b, ' ', ' ', x, ' ']).

test(even) :-
  clpfd:even(2),
  \+ clpfd:even(3).

test(every) :-
  every(clpfd:even, [2, 6, 8]),
  \+ every(clpfd:even, [2, 5, 8]).

test(fill) :-
  fill(0, x, L0),
  assertion(L0 == []),
  fill(1, x, L1),
  assertion(L1 == [x]),
  fill(2, x, L2),
  assertion(L2 == [x, x]).

test(fold) :-
  Numbers = [1, 2, 3],

  % The sum2 and sum3 predicates are defined in util.pl.
  foldl(sum2, Numbers, S1),
  assertion(S1 == 6),
  foldl(sum3(2), Numbers, 0, S2),
  assertion(S2 == 12),

  foldr(sum2, Numbers, Sum),
  assertion(Sum == 6).

test(odd) :-
  clpfd:odd(3),
  \+ clpfd:odd(2).

test(repeat) :-
  repeat(x, 0, ""),
  repeat(x, 1, "x"),
  repeat(x, 3, "xxx").

test(replace) :-
  replace([], 0, x, []),
  replace([a, b, c], 0, d, [d, b, c]),
  replace([a, b, c], 1, d, [a, d, c]).

test(some) :-
  some(clpfd:even, [1, 6, 9]),
  \+ some(clpfd:even, [1, 5, 9]).

test(tail_after_last) :-
  tail_after_last(x, [a, b, x, c, x, d, e], T),
  T =@= [d, e]. % structurally equivalent

:- end_tests(util).
:- run_tests.
:- halt.
