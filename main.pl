/*
map([], Result) :-
  Result is [].

map([Head|Tail], Result) :-
  V is Head * 2,
  format("V is ~w~n", [V]),
  map(Tail, Sub),
  Result is [V | Sub].
*/

double(X, R) :- R is X * 2.

maplist(Predicate, [H|T]) :-
  call(Predicate, H),
  maplist(Predicate, T).

maplist(_, [], []).

% [H1|T1] represents the input list.
% [H2|T2] represents the output list.
maplist(Predicate, [H1|T1], [H2|T2]) :-
    call(Predicate, H1, H2),
    maplist(Predicate, T1, T2).

main(X) :-
  L = [3, 5],
  maplist(double, L, X).
