fruit(apple).
fruit(banana).

person(mark, 74).
person(tami, 65).

tall(N) :-
  person(N, H),
  H >= 72. 

add_predicates :-
  dynamic(fruit/1),
  assertz(fruit(cherry)),

  dynamic(tall/1),
  assertz(tall(giraffe)).

report(Thing) :-
  tall(Thing) ->
    format('~w is tall.~n', [Thing]);
    format('~w is not tall.~n', [Thing]).

:- initialization
  add_predicates,

  findall(F, fruit(F), Fruits),
  format('Fruits = ~w~n', [Fruits]), % [apple, banana, cherry]

  Things = [mark, tami, giraffe],
  maplist(report, Things),
  halt.
