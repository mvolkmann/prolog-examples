% These tests can be run in Scryer Prolog
% and use my own very small unit test library.
% To run this, enter `scry -g test json.plt`
% where `scry` is your alias for `scryer-prolog`.

:- use_module(unit_test).
:- initialization(consult(json)).

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

test :-
  run_tests([
    user:test_atom,
    user:test_integer,
    user:test_list,
    user:test_pairs,
    user:test_string,
    user:test_structure
  ]),
  halt.
