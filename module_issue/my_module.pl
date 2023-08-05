:- module(my_module, [call_goals/1]).
:- use_module(library(lists)). % for maplist

call_goals(Ps) :- maplist(call, Ps).

