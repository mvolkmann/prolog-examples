fruit(apple).
fruit(banana).

person(mark, 74).
person(tami, 65).

% Rules succeed or fail.
% They do not return a value.
tall(N) :- person(N, H), H >= 72. 

add_predicates :-
  % Dynamically add a fruit fact.
  dynamic(fruit/1),
  assertz(fruit(cherry)),

  % Dynamically add a tall fact.
  dynamic(tall/1),
  assertz(tall(giraffe)),

  % There is no reason to add this particular rule dynamically,
  % but this demonstates that it is possible.
  dynamic(sum/3),
  assertz(sum(X, Y, Z) :- Z is X + Y).

report(Thing) :-
  tall(Thing) ->
    report_(Thing, 'tall');
    report_(Thing, 'not tall').

% Auxiliary rule names end in an underscore by convention.
report_(Thing, X) :- format('~w is ~w.~n', [Thing, X]).

:- initialization
  add_predicates,
  findall(F, fruit(F), Fruits),
  format('Fruits = ~w~n', [Fruits]), % [apple, banana, cherry]
  Things = [mark, tami, giraffe],
  maplist(report, Things),
  sum(2, 3, S),
  format('sum is ~w~n', [S]), % sum is 5
  halt.
