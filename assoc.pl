% This demonstrates using the assoc library in Scryer Prolog.
:- use_module(library(assoc)).
:- use_module(library(format)).

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
