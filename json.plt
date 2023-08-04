% These tests can be run in Scryer Prolog and do not use any testing framework.

:- use_module(library(clpz)).
:- use_module(library(format)).
:- initialization(consult(json)).

message(Name, Expected, Actual, Msg) :-
  ( Actual == Expected ->
    Msg = ""
  ; phrase(format_(
      "~w expected ~w but was ~w",
      [Name, Expected, Actual]
    ), Msg)
  ).

test_atom(Expected, Actual) :-
  V = foo,
  phrase(json(V), Chars),
  atom_chars(Actual, Chars),
  Expected = '"foo"'.

test_integer(Expected, Actual) :-
  V = 123,
  phrase(json(V), Actual),
  Expected = "123".

test_list(Expected, Actual) :-
  V = [foo, bar, baz],
  phrase(json(V), Chars),
  atom_chars(Actual, Chars),
  Expected = '["foo","bar","baz"]'.

test_string(Expected, Actual) :-
  V = "some text",
  phrase(json(V), Chars),
  atom_chars(Actual, Chars),
  Expected = '"some text"'.

test_structure(Expected, Actual) :-
  V = a(b, c),
  phrase(json(V), Chars),
  atom_chars(Actual, Chars),
  Expected = '{"_functor": "a/2", "_args": ["b","c"]}'.

report_count(Prefix, Count, Word) :-
  (Count #= 1 -> Noun = "test"; Noun = "tests"),
  format("~s~d ~s ~s~n", [Prefix, Count, Noun, Word]).

run_test(Test, Passed0, Passed) :-
  Goal =.. [Test, Expected, Actual],
  call(Goal),
  message(test_atom, Expected, Actual, Msg),
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

:- initialization((
  run_tests([
    test_atom, test_integer, test_list, test_string, test_structure
  ]),
  halt
)).
