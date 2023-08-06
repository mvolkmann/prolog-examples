% These tests can be run in Scryer Prolog
% and use my own very small unit test library.
% To run this, enter `scry -g test list_util.plt`
% where `scry` is your alias for `scryer-prolog`.

:- use_module(library(clpz)).
:- use_module(unit_test).
:- initialization(consult(list_util)).

is_even(N) :- mod(N, 2) =:= 0.
% is_even(N) :- R is N mod 2, R =:= 0.
% is_even(N, B) :- if_(mod(N, 2) =:= 0, B = true, B = false).
/*
is_even(N, B) :-
  R is N mod 2,
  (R #= 0 -> B = true; B = false).
*/

every1(Expected, Actual) :-
  Expected = true,
  L = [2, 6, 8],
  % (every(is_even, L) -> Actual = true; Actual = false).
  goal_bool(list_util:every(is_even, L), Actual).

every2(Expected, Actual) :-
  Expected = false,
  L = [2, 5, 8],
  goal_bool(list_util:every(is_even, L), Actual).

some1(Expected, Actual) :-
  Expected = true,
  L = [3, 6, 9],
  goal_bool(list_util:some(is_even, L), Actual).

some2(Expected, Actual) :-
  Expected = false,
  L = [3, 5, 9],
  goal_bool(list_util:some(is_even, L), Actual).

list_without1(Expected, Actual) :-
  Expected = [foo, baz],
  list_without([foo, bar, baz], bar, Actual).

list_without2(Expected, Actual) :-
  Expected = [foo, bar, baz],
  list_without([foo, bar, baz], missing, Actual).

list_without3(Expected, Actual) :-
  Expected = [],
  list_without([], foo, Actual).

test :-
  run_tests([
    user:every1,
    user:every2,
    user:some1,
    user:some2,
    user:list_without1,
    user:list_without2,
    user:list_without3
  ]),
  halt.
