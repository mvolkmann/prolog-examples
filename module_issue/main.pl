:- use_module(library(format)).
:- use_module(my_module).

% This converts a predicate to a namespaced goal
% so it can be passed to call_goals.
:- meta_predicate(predicate_goal(1, -)). % - is for output arguments
% TODO: The rules first and second take two arguments,
% TODO: so why can't I use the next line in place of the previous one?
% :- meta_predicate(predicate_goal(2, -, -)). % - is for output arguments

% TODO: Do you object to the name "predicate_goal"?
predicate_goal(G_1, G_1).

first(Expected, Actual) :-
  Expected = "one",
  Actual = "1".

second(Expected, Actual) :-
  Expected = "two",
  Actual = "2".

demo :-
  predicate_goal(first, G_1),
  predicate_goal(second, G_2),
  call_goals([G_1, G_2]).
