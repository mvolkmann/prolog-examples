% These tests can be run in Scryer Prolog and do not use any testing framework.

:- use_module(library(clpz)).
:- use_module(library(format)).
:- initialization(consult(json)).

test_atom :-
  V = foo,
  phrase(json(V), C),
  atom_chars(Actual, C),
  Expected = '"foo"',
  ( Actual == Expected ->
    true
  ; format("test_atom expected ~w but was ~w~n", [Expected, Actual]),
    fail
  ).

test_integer :-
  V = 123,
  phrase(json(V), Actual),
  Expected = "123",
  ( Actual == Expected ->
    true
  ; format("test_integer expected ~w but was ~w~n", [Expected, Actual]),
    fail
  ).

test_list :-
  V = [foo, bar, baz],
  phrase(json(V), C),
  atom_chars(Actual, C),
  Expected = '["foo","bar","baz"]',
  ( Actual == Expected ->
    true
  ; format("test_list expected ~w but was ~w~n", [Expected, Actual]),
    fail
  ).

test_string :-
  V = "some text",
  phrase(json(V), C),
  atom_chars(Actual, C),
  Expected = '"some text"',
  ( Actual == Expected ->
    true
  ; format("test_string expected ~w but was ~w~n", [Expected, Actual]),
    fail
  ).

test_structure :-
  V = a(b, c),
  phrase(json(V), C),
  atom_chars(Actual, C),
  Expected = '{"_functor": "a/2", "_args": ["b","c"]}',
  ( Actual == Expected ->
    true
  ; format("test_structure expected ~w but was ~w~n", [Expected, Actual]),
    fail
  ).

report_count(Prefix, Count, Word) :-
  (Count #= 1 -> Noun = "test"; Noun = "tests"),
  format("~s~d ~s ~s~n", [Prefix, Count, Noun, Word]).

run_test(Test, Passed0, Passed) :-
  % format("run_test: Test = ~w~n", [Test]),
  ( call(Test) ->
    Passed #= Passed0 + 1
  ; Passed #= Passed0
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

:- initialization((
  run_tests([
    test_atom, test_integer, test_list, test_string, test_structure
  ]),
  halt
)).
