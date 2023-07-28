:- use_module(library(clpz)). % for #=
:- use_module(library(format)). % for format
:- use_module(library(lists)). % for foldl

add(A, B, C) :- C #= A + B.

run :-
  Numbers = [1, 2, 3],
  foldl(add, Numbers, 0, Sum),
  format("Sum = ~d~n", [Sum]).
