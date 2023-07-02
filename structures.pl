owns(tami, pet(dog, comet)).
owns(amanda, pet(dog, maisey)).
owns(amanda, pet(dog, oscar)).
owns(jeremy, pet(dog, ramsay)).

person(mark, address('123 Some Street', 'Somewhere', 'MO', 12345)).

main :-
  owns(tami, A),
  format('pet = ~w~n', A), % pet(dog,comet)

  owns(tami, pet(dog, B)),
  format('name = ~w~n', B), % comet

  owns(tami, pet(C, D)),
  format('kind = ~w, name = ~w~n', [C, D]), % dog and comet

  person(mark, address(S, _, _, _)),
  format('street = ~w~n', S). % 123 Some Street

:- main.

