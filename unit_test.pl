:- module(unit_test, [run_tests/1]).
:- use_module(library(format)).
:- use_module(library(lists)).

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
  call(Test, Expected, Actual),
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

