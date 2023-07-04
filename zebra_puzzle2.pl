% This is Einstein's Riddle.

% There are five houses.
% Each house has a color and a number (1-5).
house(blue, _).
% The green house is immediately to the right of the ivory house.
house(green, Gn) :- house(ivory, In), Gn = In + 1.
house(ivory, _).
house(red, _).
house(yellow, _).

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
relation(englishman, H, _, _, _) :- H = house(red, _).

% The Spaniard owns the dog.
relation(spaniard, _, _, _, dog).

% Coffee is drunk in the green house.
relation(_, H, coffee, _, _) :- H = house(green, _).

% The Ukrainian drinks tea.
relation(ukrainian, _, tea, _, _).

% The Old Gold smoker owns snails.
relation(_, _, _, old_gold, snails).

% Kools are smoked in the yellow house.
relation(_, H, _, kools, _) :- H = house(yellow, _).

% Milk is drunk in the middle house.
relation(_, H, milk, _, _) :- H = house(_, 3).

% The Norwegian lives in the first house.
relation(norwegian, H, _, _, _) :- H = house(_, 1).

% The man who smokes Chesterfields lives in
% the house next to the man with the fox.
relation(_, house(_, N1), _, chesterfields, _) :-
  relation(_, house(_, N2), _, _, fox),
  (N1 = N2 - 1; N1 = N2 + 1).

% Kools are smoked in the house next to the house where the horse is kept.
relation(_, house(_, N1), _, kools, _) :-
  relation(_, house(_, N2), _, _, horse),
  (N1 = N2 - 1; N1 = N2 + 1).

% The Lucky Strike smoker drinks orange juice.
relation(_, _, orange_juice, lucky_strike, _).

% The Japanese smokes Parliaments.
relation(japanese, _, _, parliaments, _).

% The Norwegian lives next to the blue house.
relation(norwegian, house(_, N1), _, _, _) :-
  relation(_, house(blue, N2), _, _, horse),
  (N1 = N2 - 1; N1 = N2 + 1).

% Determine who drinks water and who owns the zebra.
:- relation(N1, _, water, _, _),
   format("The person that drinks water is ~w.~n", N1),
   relation(N2, _, _, _, zebra),
   format("The person that owns the zebra is ~w.~n", N2),
   halt.

