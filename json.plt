% These tests can be run in Scryer Prolog and do not use any testing framework.

:- use_module(library(format)).
:- initialization(consult(json)).

test_atom :-
  V = foo,
  phrase(json(V), C),
  atom_chars(A, C),
  ( A = '"foo"' ->
    write('test_atom passed')
  ; write('test_atom failed')
  ),
  nl.

test_integer :-
  V = 123,
  phrase(json(V), C),
  ( C = "123" ->
    write('test_integer passed')
  ; write('test_integer failed')
  ),
  nl.

test_list :-
  V = [foo, bar, baz],
  phrase(json(V), C),
  atom_chars(A, C),
  ( A = '["foo","bar","baz"]' ->
    write('test_list passed')
  ; write('test_list failed')
  ),
  nl.

test_string :-
  V = "some text",
  phrase(json(V), C),
  atom_chars(A, C),
  ( A = '"some text"' ->
    write('test_string passed')
  ; write('test_string failed')
  ),
  nl.

test_structure :-
  V = a(b, c),
  phrase(json(V), C),
  atom_chars(A, C),
  ( A = '{"_functor": "a/2", "_args": ["b","c"]}' ->
    write('test_structure passed')
  ; write('test_structure failed')
  ),
  nl.

:- initialization((
  test_atom,
  test_integer,
  test_list,
  test_string,
  test_structure
)).
