% This demonstrates using the forall predicate.

:- use_module(library(format)).
:- use_module(library(iso_ext)).

all_even(L) :- forall(member(N, L), N mod 2 =:= 0).

demo1 :-
  L = [2, 4, 8],
  ( all_even(L) ->
    write(yes)
  ; write(no)
  ).

dog(comet, whippet).
dog(maisey, treeing_walker_coonhound).
dog(oscar, german_shorthaired_pointer).
dog(ramsay, native_american_indian_dog).

demo2 :-
  forall(
    dog(Name, Breed),
    format("~w is a ~w.~n", [Name, Breed])
  ).
  
