:- use_module(my_module).

:- meta_predicate(goal0_qualified(0, -)).
goal0_qualified(G0, G0).

first :- write('in first'), nl.

second :- write('in second'), nl.

demo :-
  goal0_qualified(first, G1),
  goal0_qualified(second, G2),
  call_goals([G1, G2]).
