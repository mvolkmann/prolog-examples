:- use_module(library(format)).
:- use_module(library(lists)).

room(a, [b, c]).
room(b, [d, e]).
room(c, [f, g]).
room(d, []).
room(e, []).
room(f, []).
room(g, []).

% Find the shortest path from a starting room to a goal room.
solve(Start, Goal, Path) :- search(Goal, [], Path, Start).

search(Goal, P0, P, Start) :-
  format("evaluating room ~w~n", [Start]),
  append(P0, [Goal], P),
  (Start == Goal ->
    write(solved), nl;
    room(Start, Adjacent),
    maplist(search(Goal, P, P1), Adjacent)
  ).

:- initialization((
  solve(a, g, Path),
  write(Path), nl
)).
