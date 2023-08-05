% These tests can be run in Scryer Prolog
% and use my own very small unit test library.
% To run this, enter `scry -g test list_util.plt`
% where `scry` is your alias for `scryer-prolog`.

:- use_module(unit_test).
:- initialization(consult(list_util)).

goal_bool(List, B) :-
  Goal =.. List,
  (call(Goal) -> B = true; B = false).

is_even(N) :- mod(N, 2) =:= 0.

test_every1(Expected, Actual) :-
  Expected = true,
  L = [2, 6, 8],
  goal_bool([every, user:is_even, L], Actual).

test_every2(Expected, Actual) :-
  Expected = false,
  L = [2, 5, 8],
  goal_bool([every, user:is_even, L], Actual).

test_some1(Expected, Actual) :-
  Expected = true,
  L = [3, 6, 9],
  goal_bool([some, user:is_even, L], Actual).

test_some2(Expected, Actual) :-
  Expected = false,
  L = [3, 5, 9],
  goal_bool([some, user:is_even, L], Actual).

test_list_without1(Expected, Actual) :-
  Expected = [foo, baz],
  list_without([foo, bar, baz], bar, Actual).

test_list_without2(Expected, Actual) :-
  Expected = [foo, bar, baz],
  list_without([foo, bar, baz], missing, Actual).

test_list_without3(Expected, Actual) :-
  Expected = [],
  list_without([], foo, Actual).

test :-
  run_tests([
    user:test_every1,
    user:test_every2,
    user:test_some1,
    user:test_some2,
    user:test_list_without1,
    user:test_list_without2,
    user:test_list_without3
  ]),
  halt.
