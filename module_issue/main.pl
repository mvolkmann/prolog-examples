:- use_module(my_module).

:- meta_predicate(predicate0_goal(0, -)).
predicate0_goal(G, G).

first :- write('in first'), nl.

second :- write('in second'), nl.

demo :-
  predicate0_goal(first, G1),
  predicate0_goal(second, G2),
  call_goals([G1, G2]).
