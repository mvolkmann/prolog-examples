/*
We have three jugs labeled a, b, and c
with capacities 4, 3, and 7 respectively.
We can pour water from any non-empty jug to any non-full jug.
When we do, we just pour as much as possible.
The goal is to find a sequence of pours
that result in a jug containing 2 units.

The jug structure arguments are number, capacity, and current level.
The from_to structure arguments are from jug number and to jug number.
The solution is a list of from_to structures.
*/

%:- use_module(library(clpfd)). % only for SWI-Prolog
:- use_module(library(clpz)). % only for Scryer Prolog

% A solution has been found when any jug contains 2 units.
% Jugs0 is a list of three jug structures.
moves(Jugs0) --> { member(jug(_, _, 2), Jugs0) }.

moves(Jugs0) -->
  [from_to(From, To)],
  {
    select(jug(From, FromCapacity, FromFill0), Jugs0, Jugs1),
    select(jug(To, ToCapacity, ToFill0), Jugs1, Jugs),

    % Calculate the number of units that can be moved
    % from the From jug to the To jug.
    Amount #= min(FromFill0, ToCapacity - ToFill0),

    % Calculate the new amount in the From jug.
    FromFill #= FromFill0 - Amount,

    % Calculate the new amount in the To jug.
    ToFill #= ToFill0 + Amount 
  },
  moves([
    jug(From, FromCapacity, FromFill),
    jug(To, ToCapacity, ToFill) | Jugs
  ]).

print_move(from_to(F, T)) :-
  format('Pour from ~w to ~w.~n', [F, T]).

:- initialization((
   length(Moves, L), % for iterative deepening
   phrase(moves([jug(a,4,0), jug(b,3,0), jug(c,7,7)]), Moves),
   format('The solution requires ~d moves.~n', [L]),
   maplist(print_move, Moves),
   halt
 )).
