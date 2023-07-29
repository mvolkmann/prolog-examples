% This demonstrates using the assoc library in Scryer Prolog.
:- use_module(library(assoc)).
:- use_module(library(format)).

demo :-
  empty_assoc(A0),
  put_assoc(name, A0, 'Mark', A1),
  format("A1 = ~w~n", [A1]),
  get_assoc(name, A1, Name),
  format("Name = ~w~n", [Name]),
  % del_assoc(name, A1, 'Mark', A2),
  del_assoc(name, A1, _, A2),
  format("A2 = ~w~n", [A2]).

:- initialization((
  % empty_assoc(A),
  Pairs = [a-apple, b-banana, c-cherry],
  list_to_assoc(Pairs, A),
  get_assoc(a, A, Word1),
  Word1 == apple,
  get_assoc(b, A, Word2),
  Word2 == banana,
  get_assoc(c, A, Word3),
  Word3 == cherry,
  put_assoc(b, A, blueberry, A2),
  get_assoc(b, A2, Word4),
  Word4 == blueberry
  /*
  format("Word = ~w~n", [Word1]),
  format("Word = ~w~n", [Word2]),
  format("Word = ~w~n", [Word3])
  */
)).
