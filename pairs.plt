% To run these tests, enter `swipl pairs.plt`

:- begin_tests(pairs).
:- use_module(library(pairs)).

test(pairs_keys_values) :-
  pairs_keys_values([a-apple, b-banana, c-cherry], Ks, Vs),
  assertion(Ks = [a, b, c]),
  assertion(Vs = [apple, banana, cherry]).

test(pairs_keys) :-
  pairs_keys([a-apple, b-banana, c-cherry], Ks),
  assertion(Ks = [a, b, c]).

test(pairs_values) :-
  pairs_values([a-apple, b-banana, c-cherry], Vs),
  assertion(Vs = [apple, banana, cherry]).

test(group_pairs_by_key) :-
  % The pairs must be sorted on their keys.
  group_pairs_by_key([a-apple, a-ape, b-banana, b-bear], Gs),
  assertion(Gs = [a-[apple,ape], b-[banana,bear]]).

first_letter(Atom, Letter) :- atom_chars(Atom, [Letter|_]).
test(map_list_to_pairs) :-
  map_list_to_pairs(first_letter, [apple, banana, cherry], Ps),
  assertion(Ps = [a-apple, b-banana, c-cherry]).

:- end_tests(pairs).
:- run_tests.
:- halt.
