% This demonstrates using some of the predicates inthe "apply" library.

:- use_module(library(apply)).

double(X, Y) :- Y is X * 2.
sum(N1, N2, Sum) :- Sum is N1 + N2.

:- initialization((
  Numbers = [1, 2, 3],
  foldl(sum, Numbers, 0, Sum),
  format('Sum = ~w~n', [Sum]),

  maplist(double, Numbers, Doubled),
  format('Doubled = ~w~n', [Doubled]),

  halt
)).
