% This is the "zebra puzzle", also referred to as "Einstein's Riddle".
% See https://www.101computing.net/solving-a-zebra-puzzle-using-prolog/.

% Three kids went to a superheroes fancy dress birthday party.
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

/*
TODO: Why don't these rules work instead of the ones below?
kid_hero_age(anya, spiderman, age(A)).
kid_hero_age(ethan, hero(H), age(A)) :- H \= captain_america.
kid_hero_age(kid(K), spiderman, 6).
kid_hero_age(kid(K), captain_american, 8).
*/

% Anya was dressed up as Spiderman.
kid_hero_age(K,H,A):- K=anya, H=spiderman, age(A).

% Ethan was not dressed up as Captain America.
kid_hero_age(K,H,A):- K=ethan, hero(H), age(A), H\=captain_america.

% The youngest kid dressed up as Spiderman.
kid_hero_age(K,H,A):- kid(K), H=spiderman, A=6.

% The kid who is 8 years old dressed up as Captain America.
kid_hero_age(K,H,A):- kid(K), H=captain_america, A=8.

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

% :- print("hello").
:- solve(K1, H1, A1, K2, H2, A2, K3, H3, A3),
   S = "~w is ~w and dressed as ~w.~n",
   format(S, [K1, A1, H1]),
   format(S, [K2, A2, H2]),
   format(S, [K3, A3, H3]),
   halt.
