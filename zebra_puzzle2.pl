% This is Einstein's Riddle.

% There are five houses.
% Each house has a color and a number (1-5).
house(blue, Bn).
% The green house is immediately to the right of the ivory house.
house(green, Gn) :- house(ivory, In), Gn = In + 1.
house(ivory, In).
house(red, Rn).
house(yellow, Yn).

animal(dog).
animal(fox).
animal(horse).
animal(snails).
animal(zebra).

nationality(englishman).
nationality(japanese).
nationality(norwegian).
nationality(spaniard).
nationality(ukrainian).

drink(coffee).
drink(milk).
drink(orange_juice).
drink(tea).
drink(water).

smoke(chesterfields).
smoke(kools).
smoke(lucky_strike).
smoke(old_gold).
smoke(parliaments).

% relation(nationality, house, drinks, smokes, animal)

% The Englishman lives in the red house.
relation(englishman, H, D, S, A) :- H = house(red, _).

% The Spaniard owns the dog.
relation(spaniard, H, D, S, dog).

% Coffee is drunk in the green house.
relation(N, H, coffee, S, A) :- H = house(green, _).

% The Ukrainian drinks tea.
relation(ukrainian, C, tea, S, A).

% The Old Gold smoker owns snails.
relation(N, C, D, old_gold, snails).

% Kools are smoked in the yellow house.
relation(N, H, D, kools, A) :- H = house(yellow, _).

% Milk is drunk in the middle house.
relation(N, H, milk, S, A) :- H = house(_, 3).

% The Norwegian lives in the first house.
relation(norwegian, H, D, S, A) :- H = house(_, 1).

% The man who smokes Chesterfields lives in
% the house next to the man with the fox.
relation(N, house(_, N1), D, chesterfields, A) :-
  relation(_, house(_, N2), _, _, fox),
  (N1 = N2 - 1; N1 = N2 + 1).

% Kools are smoked in the house next to the house where the horse is kept.
relation(N, house(_, N1), D, kools, A) :-
  relation(_, house(_, N2), _, _, horse),
  (N1 = N2 - 1; N1 = N2 + 1).

% The Lucky Strike smoker drinks orange juice.
relation(N, H, orange_juice, lucky_strike, A).

% The Japanese smokes Parliaments.
relation(japanese, H, D, parliaments, A).

% The Norwegian lives next to the blue house.
relation(norwegian, house(_, N1), D, S, A) :-
  relation(_, house(blue, N2), _, _, horse),
  (N1 = N2 - 1; N1 = N2 + 1).

% Determine who drinks water and who owns the zebra.
:- relation(N1, _, water, _, _),
   format("The person that drinks water is ~w.~n", N1),
   relation(N2, _, _, _, zebra),
   format("The person that owns the zebra is ~w.~n", N2),
   halt.

