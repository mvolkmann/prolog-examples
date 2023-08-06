:- module(my_module, [call_goals/1]).
:- use_module(library(format)).
:- use_module(library(lists)). % for maplist

call_goal(G) :-
  call(G, Expected, Actual),
  format("called ~w and got ~s, ~s~n", [G, Expected, Actual]).

call_goals(Gs) :- maplist(call_goal, Gs).

