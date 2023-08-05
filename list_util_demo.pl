:- use_module(list_util). % my module

list_without_demo :-
  L0= [red, green, blue],
  list_without(L0, green, L),
  format("L = ~w~n", [L]).

print_list_parts([H|T]) :-
  format("head is ~w, tail is ~w", [H, T]).

print_elements([]).

print_elements([H|T]) :-
  writeln(H),
  print_elements(T).

print_second(L) :-
  [_, S] = L,
  write(S), nl.

