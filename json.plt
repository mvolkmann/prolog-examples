% These tests can be run in Scryer Prolog and do not use any testing framework.

:- use_module(library(format)).
:- initialization(consult(json)).

test_atom :-
  V = foo,
  phrase(json(V), C),
  format("C = ~w~n", [C]),
  atom_chars(A, C),
  format("A = ~w~n", [A]),
  ( A = '"foo"' -->
    write('test_atom passed')
  ; write('test_atom failed')
  ),
  nl.

:- initialization((
  test_atom
)).
