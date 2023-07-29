:- use_module(library(clpz)). % for #=
:- use_module(library(format)). % for format
:- use_module(library(lists)). % for foldl

add(A, B, C) :- C #= A + B.

sum([], 0).
sum([H|T], Sum) :-
  sum(T, Sum0), 
  Sum is H + Sum0.

run :-
  Numbers = [1, 2, 3],

  sum(Numbers, Sum1),
  format("Sum1 = ~d~n", [Sum1]),

  foldl(add, Numbers, 0, Sum2),
  format("Sum2 = ~d~n", [Sum2]).
