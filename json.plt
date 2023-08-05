% These tests can be run in Scryer Prolog and use my own testing framework.

:- use_module(library(clpz)).
:- use_module(library(format)).
:- use_module(library(lists)). % for foldl
:- use_module(unit_test).
:- initialization(consult(json)).

% This converts a predicate to a namespaced goal
% that can be passed to call_goals.
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

% :- initialization((
run :-
  predicate_goal(test_atom, G1),
  predicate_goal(test_integer, G2),
  predicate_goal(test_list, G3),
  predicate_goal(test_string, G4),
  predicate_goal(test_structure, G5),
  run_tests([G1, G2, G3, G4, G5]),
  halt.
% )).
