:- use_module(library(clpfd)).

:- initialization
  X in 5..10, Y in 7..14, X #> Y, label([X, Y]).
