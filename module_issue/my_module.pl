:- module(my_module, [call_predicates/1]).
:- use_module(library(lists)).

call_predicate(P) :-
  Goal =.. [P],
  call(Goal).

call_predicates(Ps) :- maplist(call_predicate, Ps).

