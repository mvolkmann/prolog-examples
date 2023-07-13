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

test(fill) :-
  fill(0, x, L0),
  assertion(L0 == []),
  fill(1, x, L1),
  assertion(L1 == [x]),
  fill(2, x, L2),
  assertion(L2 == [x, x]).

test(fold) :-
  Numbers = [1, 2, 3],

  % The add predicate is defined in util.pl.
  % foldl(add, Numbers, Sum),
  foldl(mystery(99), Numbers, 0, Sum),
  format('Sum = ~w~n', [Sum]),
  assertion(Sum == 6),

  foldr(add, Numbers, Sum),
  assertion(Sum == 6).

test(repeat) :-
  repeat(x, 0, ""),
  repeat(x, 1, "x"),
  repeat(x, 3, "xxx").

test(replace) :-
  replace([], 0, x, []),
  replace([a, b, c], 0, d, [d, b, c]),
  replace([a, b, c], 1, d, [a, d, c]).

test(tail_after_last) :-
  tail_after_last(x, [a, b, x, c, x, d, e], T),
  T =@= [d, e]. % structurally equivalent

:- end_tests(util).
:- run_tests.
:- halt.
