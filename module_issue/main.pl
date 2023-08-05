:- use_module(my_module).

first :- write('in first'), nl.

second :- write('in second'), nl.

:- meta_predicate first(0)
:- meta_predicate second(0)

demo :-
  call_predicates([first, second]).
