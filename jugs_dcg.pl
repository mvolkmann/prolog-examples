/*
We have three jugs labeled a, b, and c
with capacities 4, 3, and 7 respectively.
We can pour water from any non-empty jug to any non-full jug.
When we do, we just pour as much as possible.
The goal is to find a sequence of pours
that result in a jug containing 2 units.

The jug structure arguments are label, capacity, and current level.
The from_to structure arguments are from jug number and to jug number.
The solution is a list of from_to structures.
*/

:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).

% A solution has been found when any jug contains 2 units.
% Jugs is a list of three jug structures.
pours(Jugs) --> { member(jug(_, _, 2), Jugs) }.

% This generates a list of from_to structures
% that describe the solution.
% Jugs0 is a list of three jug structures in no particular order.
pours(Jugs0) -->
  % This will choose values for From and To
  % which will be one of the labels a, b, or c.
  % TODO: How does it do this? Must be the Prolog search engine.
  [from_to(From, To)],
  {
    % format("evaluating pour from ~w to ~w~n", [From, To]),
    % Find the jug that is not the from or to jug
    % by creating a list that does not contain the from jug
    % and then creating a list that does not contain the to jug.
    select(jug(From, FromCapacity, FromFill0), Jugs0, Jugs1),
    select(jug(To, ToCapacity, ToFill0), Jugs1, Jugs),
    format("From = ~w, To = ~w, Jugs =  ~w~n", [From, To, Jugs]),

    % Calculate the number of units that can be moved
    % from the From jug to the To jug.
    Amount #= min(FromFill0, ToCapacity - ToFill0),

    % Calculate the new amount in the From jug.
    FromFill #= FromFill0 - Amount,

    % Calculate the new amount in the To jug.
    ToFill #= ToFill0 + Amount 
  },
  % Create a new list of three jug structures
  % that represent the new state.
  pours([
    jug(From, FromCapacity, FromFill),
    jug(To, ToCapacity, ToFill) | Jugs
  ]).

print_pour(from_to(F, T)) :-
  format("Pour from ~w to ~w.~n", [F, T]).

:- initialization((
   length(Pours, L), % for iterative deepening
   phrase(pours([jug(a,4,0), jug(b,3,0), jug(c,7,7)]), Pours),
   format("The solution requires ~d moves.~n", [L]),
   maplist(print_pour, Pours),
   halt
 )).
