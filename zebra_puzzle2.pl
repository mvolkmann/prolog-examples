/*
This is Einstein's Riddle.

There are five animals:
dog, fox, horse, snails, and zebra.

There are five nationalities:
englishman, japanese, norwegian, spaniard, and ukrainian.

There are five drinks:
coffee, milk, orange_juice, tea, and water.

There are five cigarettes:
chesterfields, kools, lucky_strike, old_gold, and parliaments.

There are five houses.
Each house has a color and a number (1-5).

relation rules have the following arguments:
nationality, house, drinks, smokes, and animal.
*/

% The green house is immediately to the right of the ivory house.
house(green, Gn) :- house(ivory, In), Gn = In + 1.

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
  relation(_, house(blue, N2), _, _, _),
  (N1 = N2 - 1; N1 = N2 + 1).

% Someone drinks water.
relation(_, _, water, _, _).

% Someone owns a zebra.
relation(_, _, _, _, zebra).

% Determine who drinks water and who owns the zebra.
:- S = "~w ~w ~w ~w ~w~n",
   relation(N1, house(C1, 1), D1, S1, A1),
   format(S, [C1, N1, D1, S1, A1]),
   relation(N2, house(C2, 2), D2, S2, A2),
   format(S, [C2, N2, D2, S2, A2]),
   relation(N3, house(C3, 3), D3, S3, A3),
   format(S, [C3, N3, D3, S3, A3]),
   relation(N1, house(C1, 1), D1, S1, A1),
   format(S, [C1, N1, D1, S1, A1]),
   relation(N5, house(C5, 5), D5, S5, A5),
   format(S, [C5, N5, D5, S5, A5]),
   halt.

