% To run these tests, enter `swipl pairs.plt`

:- consult(pairs).
:- begin_tests(pairs).
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

test(pairs_keys_values) :-
  pairs_keys_values([a-apple, b-banana, c-cherry], Ks, Vs),
  assertion(Ks = [a, b, c]),
  assertion(Vs = [apple, banana, cherry]).

test(pairs_keys) :-
  pairs_keys([a-apple, b-banana, c-cherry], Ks),
  assertion(Ks = [a, b, c]).

test(pairs_values) :-
  pairs_values([a-apple, b-banana, c-cherry], Ks),
  assertion(Ks = [apple, banana, cherry]).

test(group_pairs_by_key) :-
  % The pairs must be sorted on their keys.
  group_pairs_by_key([a-apple, a-ape, b-banana, b-bear], Gs),
  assertion(Gs = [a-[apple,ape], b-[banana,bear]]).

:- end_tests(pairs).
:- run_tests.

% This doesn't seem to generate a useful report.
% :- show_coverage(pairs, [dir(cov)]).
:- halt.
