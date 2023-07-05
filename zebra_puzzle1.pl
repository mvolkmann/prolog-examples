% Three kids went to a superheroes dress birthday party.
% This is the "zebra puzzle", also referred to as "Einstein's Riddle".
% See https://www.101computing.net/solving-a-zebra-puzzle-using-prolog/.

/*
% The names of the three kids are Ethan, Ali and Anya.
kid(ethan).
kid(ali).
kid(anya).

% They dressed up as Spiderman, Captain America and Iron Man.
hero(spiderman).
hero(captain_america).
hero(iron_man).

% The kids are 6, 8 and 10 years old.
age(6).
age(8).
age(10).

% Anya was dressed up as Spiderman.
kid_hero_age(anya, spiderman, A) :- age(A).

% Ethan was not dressed up as Captain America.
kid_hero_age(ethan, H, A) :- hero(H), age(A), H\=captain_america.

% The youngest kid dressed up as Spiderman.
kid_hero_age(K, spiderman, 6):- kid(K).

% The kid who is 8 years old dressed up as Captain America.
kid_hero_age(K, captain_america, 8) :- kid(K).

% Three values are distinct if this holds.
different(A, B, C) :-
  A \= B, A \= C, B \= C. % use distinct list?

% Determine the missing information.
solve(K1, H1, A1, K2, H2, A2, K3, H3, A3) :-
  kid_hero_age(K1, H1, A1),
  kid_hero_age(K2, H2, A2),
  kid_hero_age(K3, H3, A3),
  different(K1, K2, K3),
  different(H1, H2, H3),
  different(A1, A2, A3),
  !.

:- solve(K1, H1, A1, K2, H2, A2, K3, H3, A3),
   S = "~w is ~w and dressed as ~w.~n",
   format(S, [K1, A1, H1]),
   format(S, [K2, A2, H2]),
   format(S, [K3, A3, H3]),
   halt.
*/

/* The output is:
   anya is 6 and dressed as spiderman.
   ethan is 10 and dressed as iron_man.
   ali is 8 and dressed as captain_america.
*/

% This is an alternate strategy that is
% similar to that used in zebra_puzzle2.pl.
solve(Ks) :-
  length(Ks, 3),

  % Anya was dressed up as Spiderman.
  member(relation(anya, spiderman, _), Ks),

  % Ethan was not dressed up as Captain America.
  % The freeze predicate delays execution of a goal
  % until a given variable is bound.
  % member(relation(ethan, H, _), Ks), freeze(H, H \= captain_america),
  % The dif predicate removes the need for freeze.
  member(relation(ethan, H, _), Ks), dif(H, captain_america),

  % The youngest kid dressed up as Spiderman.
  member(relation(_, spiderman, 6), Ks),

  % The kid who is 8 years old dressed up as Captain America.
  member(relation(_, captain_america, 8), Ks),

  % Some kid is named ali.
  member(relation(ali, _, _), Ks),

  % Some kid dressed as iron_man.
  member(relation(_, iron_man, _), Ks),

  % Some kid is 10.
  member(relation(_, _, 10), Ks).

print_kid(relation(Name, Hero, Age)) :-
   format("~w is ~w and dressed as ~w.~n", [Name, Hero, Age]).

:- solve(Ks), maplist(print_kid, Ks), halt.
