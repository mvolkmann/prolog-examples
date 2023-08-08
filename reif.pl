:- use_module(library(format)).
:- use_module(library(reif)).

dog(comet).
dog(maisey).
dog(oscar).
dog(ramsay).

writeln(X) :- write(X), nl.

goal_bool(Goal, Bool) :- Goal -> Bool = true; Bool = false.

is_dog(X, B) :- goal_bool(dog(X), B).

report_reif(Name) :-
  % The first argument must be a predicate that accepts
  % an extra variable argument to receive true or false.
  if_(
    is_dog(Name),
    writeln(dog),
    writeln('not a dog')
  ).

demo :-
  report_reif(comet), % dog
  report_reif(mark), % not a dog

  Beings = [mark, comet, tami, maisey, ramsay, oscar],

  tfilter(is_dog, Beings, JustDogs),
  format("JustDogs = ~w~n", [JustDogs]), % [comet,maisey,ramsay,oscar]

  tpartition(is_dog, Beings, Dogs, NonDogs),
  format("dogs include ~w~n", [Dogs]), % [comet,maisey,ramsay,oscar]
  format("non-dogs include ~w~n", [NonDogs]). % [mark,tami]
