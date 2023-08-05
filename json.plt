% These tests can be run in Scryer Prolog and do not use any testing framework.

:- use_module(library(clpz)).
:- use_module(library(format)).
:- use_module(library(lists)). % for foldl
:- use_module(unit_test).
:- initialization(consult(json)).

% This converts a predicate that takes one argument
% to a namespaced goal that can be passed to call_goals.
:- meta_predicate(predicate_goal(1, -)).
predicate_goal(G, G).

test_atom(Expected, Actual) :-
  Expected = '"foo"',
  V = foo,
  phrase(json(V), Chars),
  atom_chars(Actual, Chars).

test_integer(Expected, Actual) :-
  Expected = "123",
  V = 123,
  phrase(json(V), Actual).

test_list(Expected, Actual) :-
  Expected = '["foo","bar","baz"]',
  V = [foo, bar, baz],
  phrase(json(V), Chars),
  atom_chars(Actual, Chars).

test_pairs(Expected, Actual) :-
  Expected = '{ "red": "stop", "green": "go", "yellow": "yield" }',
  V = [red-stop, green-go, yellow-yield],
  phrase(json(V), Chars),
  atom_chars(Actual, Chars).

test_string(Expected, Actual) :-
  Expected = '"some text"',
  V = "some text",
  phrase(json(V), Chars),
  atom_chars(Actual, Chars).

test_structure(Expected, Actual) :-
  Expected = '{"_functor": "a/2", "_args": ["b","c"]}',
  V = a(b, c),
  phrase(json(V), Chars),
  atom_chars(Actual, Chars).

:- initialization((
  predicate_goal(test_atom, G1),
  predicate_goal(test_integer, G2),
  predicate_goal(test_list, G3),
  predicate_goal(test_string, G4),
  predicate_goal(test_structure, G5),
  run_tests([G1, G2, G3, G4, G5]),
  halt
)).

/*
% TODO: The remaining predicates should be defined in unit_test.pl,
% TODO: but code doesn't work if I do that.
% TODO: See https://github.com/mthom/scryer-prolog/discussions/1951
message(Name, Expected, Actual, Msg) :-
  ( Actual == Expected ->
    Msg = ""
  ; phrase(format_(
      "~w expected ~w but was ~w",
      [Name, Expected, Actual]
    ), Msg)
  ).

report_count(Prefix, Count, Word) :-
  (Count #= 1 -> Noun = "test"; Noun = "tests"),
  format("~s~d ~s ~s~n", [Prefix, Count, Noun, Word]).

run_test(Test, Passed0, Passed) :-
  Goal =.. [Test, Expected, Actual],
  call(Goal),
  message(Test, Expected, Actual, Msg),
  length(Msg, Length),
  (Length #= 0 ->
    Passed #= Passed0 + 1
  ; format("~s~n", [Msg]),
    Passed #= Passed0
  ).

run_tests(Tests) :-
  foldl(run_test, Tests, 0, Passed),
  length(Tests, Length),
  (Passed #= Length -> Prefix = "All "; Prefix = ""),
  report_count(Prefix, Passed, "passed"),
  Failed #= Length - Passed,
  (Failed #= 0 ->
    true
  ; report_count("", Failed, "failed")
  ).
*/
