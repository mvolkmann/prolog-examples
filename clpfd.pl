:- use_module(library(clpfd)).

add(A, B, C) :- C #= A + B.

:- initialization
  X in 5..10, Y in 7..14, X #> Y, label([X, Y]).
