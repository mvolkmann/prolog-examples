:- use_module(library(format)).
:- use_module(my_module).

% This converts a predicate that takes one argument
% to a namespaced goal that can be passed to call_goals.
:- meta_predicate(predicate_goal(1, -)).
predicate_goal(G, G).

first(Expected, Actual) :-
  format("first: ~w ~w~n", [Expected, Actual]).

second(Expected, Actual) :-
  format("second: ~w ~w~n", [Expected, Actual]).

demo :-
  predicate_goal(first, G1),
  predicate_goal(second, G2),
  call_goals([G1, G2]).
