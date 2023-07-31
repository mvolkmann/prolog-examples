:- use_module(library(format)).

max_member(List, Max) :-
  member(Max, List), % Max is a member of List.
  % It is not true that there is any member of List
  % whose value is greater than Max.
  \+ (member(E, List), E > Max).

max([H|T], Max) :- max_(T, H, Max).
max_([], Max, Max).
max_([H|T], Max0, Max) :-
  H > Max0, max_(T, H, Max).
max_([H|T], Max0, Max) :-
  H =< Max0, max_(T, Max0, Max).

demo :-
  Numbers = [6, 2, 7, 9, 5, 1],
  % max_member(Numbers, Max),
  max(Numbers, Max),
  format("Max = ~w~n", [Max]).

