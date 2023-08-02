:- use_module(library(clpz)).
:- initialization(consult(family)).

findall_demo(L) :-
  findall(X, grandfather(richard, X), L).
  % L = [amanda, jeremy]

add(A, B, C) :- C #= A + B.

foldl_demo(Sum) :-
  Numbers = [1, 2, 3],
  foldl(add, Numbers, 0, Sum).
  % Sum = 6

list_max_demo(Max) :-
  Numbers = [1, 2, 3],
  list_max(Numbers, Max).
  % Max = 3

list_min_demo(Min) :-
  Numbers = [1, 2, 3],
  list_min(Numbers, Min).
  % Min = 1

maplist_demo(Ls) :-
  Fruits = ["apple", "banana", "cherry"],
  maplist(length, Fruits, Ls).
  % Ls = [5, 6, 6] 

nth0_demo(E, Es) :-
  Fruits = [apple, banana, cherry],
  nth0(0, Fruits, E, Es).
  % E = apple, Es = [banana,cherry]

reverse_demo(R) :-
  Numbers = [1, 2, 3],
  reverse(Numbers, R).
  % R = [3, 2, 1]

select_demo(L) :-
  Numbers = [1, 2, 3],
  select(2, Numbers, L).
  % L = [1, 3]

sum_list_demo(Sum) :-
  Numbers = [1, 2, 3],
  sum_list(Numbers, Sum).
  % Sum = 6
