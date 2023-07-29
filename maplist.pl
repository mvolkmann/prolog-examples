:- use_module(library(lists)).

double(A, B) :-
  ( var(A) ->
    A is B / 2
  ; B is A * 2
  ).

:- initialization((
  maplist(double, [1, 2, 3], L),
  write(L), nl, % output is [2,4,6]
  halt
)).
