:- use_module(library(format)).
:- use_module(my_module).

% This converts a predicate to a namespaced goal
% that can be passed to call_goals.
:- meta_predicate(predicate_goal(1, -)).
predicate_goal(G, G).

first(Expected, Actual) :-
  Expected = '"one"',
  Actual = '"wrong"'.

second(Expected, Actual) :-
  Expected = '"two"',
  Actual = '"wrong"'.

demo :-
  predicate_goal(first, G1),
  predicate_goal(second, G2),
  format("~w ~w~n", [G1, G2]),
  call_goals([G1, G2]).
