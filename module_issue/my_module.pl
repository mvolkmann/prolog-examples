:- module(my_module, [call_goals/1]).
:- use_module(library(format)).
:- use_module(library(lists)). % for maplist

call_goal(G) :-
  format("calling ~w~n", [G]),
  call(G, alpha, beta).

call_goals(Gs) :- maplist(call_goal, Gs).

