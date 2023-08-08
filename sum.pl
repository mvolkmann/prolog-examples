:- use_module(library(clpz)). % for #=
:- use_module(library(format)). % for format
:- use_module(library(lists)). % for foldl and sum_list

sum1([], 0).
sum1([H|T], Sum) :- sum1(T, Sum0), Sum is H + Sum0.

add(A, B, C) :- C #= A + B.
sum2(Numbers, Sum) :- foldl(add, Numbers, 0, Sum).

demo :-
  Numbers = [1, 2, 3],

  sum1(Numbers, Sum1),
  format("Sum1 = ~d~n", [Sum1]),

  sum2(Numbers, Sum2),
  format("Sum2 = ~d~n", [Sum2]),

  sum_list(Numbers, Sum3),
  format("Sum3 = ~d~n", [Sum3]).
