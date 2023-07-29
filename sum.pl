:- use_module(library(clpz)). % for #=
:- use_module(library(format)). % for format
:- use_module(library(lists)). % for foldl

add(A, B, C) :- C #= A + B.

% Using recursion.
sum_recursion([], 0).
sum_recursion([H|T], Sum) :-
  sum_recursion(T, Sum0), 
  Sum is H + Sum0.

% Using repeat predicate.
sum_repeat(Numbers, Sum) :-
  Sum = 0,
  repeat,
    [H|T] = Numbers,
    Sum is Sum + H,
    Numbers = T,
  !.

run :-
  Numbers = [1, 2, 3],

  sum_recursion(Numbers, Sum1),
  format("Sum1 = ~d~n", [Sum1]),

  foldl(add, Numbers, 0, Sum3),
  format("Sum3 = ~d~n", [Sum3]).
