% To run these tests, enter `swipl pairs.plt`

:- consult(queen).
:- begin_tests(queen).
:- use_module(library(pairs)).

test(same_column) :-
  queen_can_attack((3,2), (7,2)),
  \+ queen_can_attack((3,2), (7,5)),
  !.

test(same_diagonal) :-
  queen_can_attack((3,2), (5,4)),
  queen_can_attack((5,4), (3,2)),
  queen_can_attack((3,4), (5,2)),
  queen_can_attack((5,2), (3,4)),
  \+ queen_can_attack((3,2), (7,5)),
  !.

test(same_row) :-
  queen_can_attack((2,3), (2,7)),
  \+ queen_can_attack((2,3), (5,7)),
  !.

test(same_column) :-
  qca(3-2, 7-2),
  \+ qca(3-2, 7-5),
  !.

test(same_diagonal) :-
  qca(3-2, 5-4),
  qca(5-4, 3-2),
  qca(3-4, 5-2),
  qca(5-2, 3-4),
  \+ qca(3-2, 7-5),
  !.

test(same_row) :-
  qca(2-3, 2-7),
  \+ qca(2-3, 5-7),
  !.

:- end_tests(pairs).
:- run_tests.

% This doesn't seem to generate a useful report.
% :- show_coverage(pairs, [dir(cov)]).
:- halt.
