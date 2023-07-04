/*
This solves Einstein's Riddle.

There are five nationalities:
englishman, japanese, norwegian, spaniard, and ukrainian.

There are five houses colors:
blue, green, ivory, red, and yellow.

There are five drinks:
coffee, milk, orange_juice, tea, and water.

There are five smokes:
chesterfields, kools, lucky_strike, old_gold, and parliaments.

There are five animals:
dog, fox, horse, snails, and zebra.
*/

% relation(Nationality, Color, Drinks, Smokes, Pet).

% Two list elements are adjacent if one of the following holds.
% Appending something to a list beginning with A,B results in a given list.
adjacent(A, B, Ls) :- append(_, [A,B|_], Ls).
% Appending something to a list beginning with B,A results in a given list.
adjacent(A, B, Ls) :- append(_, [B,A|_], Ls).

houses(Hs) :-
  % There are five houses.
  length(Hs, 5),

  % The Englishman lives in the red house.
  member(relation(englishman, red, _, _, _), Hs),

  % The Spaniard owns the dog.
  member(relation(spaniard, _, _, _, dog), Hs),

  % Coffee is drunk in the green house.
  member(relation(_, green, coffee, _, _), Hs),

  % The Ukrainian drinks tea.
  member(relation(ukrainian, _, tea, _, _), Hs),

  % The green house is next to the ivory house.
  adjacent(
    relation(_, green, _, _, _),
    relation(_, ivory, _, _, _),
    Hs),

  % The Old Gold smoker owns snails.
  member(relation(_, _, _, old_gold, snails), Hs),

  % Kools are smoked in the yellow house.
  member(relation(_, yellow, _, kools, _), Hs),

  % Milk is drunk in the middle house.
  Hs = [_, _, relation(_, _, milk, _, _), _, _],

  % The Norwegian lives in the first house.
  Hs = [relation(norwegian, _, _, _, _) | _],

  % The man who smokes Chesterfields lives in
  % the house next to the man with the fox.
  adjacent(
    relation(_, _, _, chesterfields, _),
    relation(_, _, _, _, fox),
    Hs),

  % Kools are smoked in the house next to the house where the horse is kept.
  adjacent(
    relation(_, _, _, kools, _),
    relation(_, _, _, _, horse),
    Hs),

  % The Lucky Strike smoker drinks orange juice.
  member(relation(_, _, orange_juice, lucky_strike, _), Hs),

  % The Japanese smokes Parliaments.
  member(relation(japanese, _, _, parliaments, _), Hs),

  % The Norwegian lives next to the blue house.
  adjacent(
    relation(norwegian, _, _, _, _),
    relation(_, blue, _, _, _),
    Hs),

  % Someone drinks water.
  member(relation(_, _, water, _, _), Hs),

  % Someone owns a zebra.
  member(relation(_, _, _, _, zebra), Hs).

zebra_owner(Owner) :-
	houses(Hs),
	member(relation(Owner,zebra,_,_,_), Hs).

water_drinker(Drinker) :-
	houses(Hs),
	member(relation(Drinker,_,_,water,_), Hs).

print_houses([]).

print_houses([H|T]) :-
  [N, C, D, S, P] = H,
  format("~w ~w ~w ~w ~w~n", [C, N, D, S, P]),
  print_houses(T).

  % :- houses(Hs), print_houses(Hs).
