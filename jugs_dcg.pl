/*
We have three jugs labeled a, b, and c
with capacities 4, 3, and 7 respectively.
We can pour water from any non-empty jug to any non-full jug.
When we do, we just pour as much as possible.
The goal is to find a sequence of pours
that result in a jug containing 2 units.

The jug structure arguments are label, capacity, and current level.
The from_to structure arguments are from jug label and to jug label.
The solution is a list of from_to structures.
*/

:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).

% Jugs is a list of three jug structures.
% A solution has been found when any jug contains 2 units.
pours(Jugs) --> { member(jug(_, _, 2), Jugs) }.

% Jugs0 is a list of three jug structures in no particular order.
% This generates a list of from_to structures that describe a solution.
pours(Jugs0) -->
  % This will choose values for From and To
  % which will be one of the labels a, b, or c.
  % The Prolog search engine will try all possible combinations
  % which are a to b, a to c, b to a, b to c, c to a, and c to b.
  [from_to(From, To)],
  {
    % Find jug that is not the from or to jug
    % by creating a list that does not contain from jug and
    % then creating another list that does not contain to jug.
    % This is where pours from a jug to itself are filtered out.
    % If From and To are both set to the same jug label then
    % the first select will succeed, but the second will fail.
    select(jug(From, FromCapacity, FromFill0), Jugs0, Jugs1),
    select(jug(To, ToCapacity, ToFill0), Jugs1, Other),
    % format("Using From = ~w, To = ~w, Other =  ~w~n", [From, To, Other]),
    % Other is a list containing one jug structure that will
    % be used as the tail of a new list created below.

    % Calculate units that can be moved from From jug to To jug.
    Amount #= min(FromFill0, ToCapacity - ToFill0),

    % Calculate new amount in From jug.
    FromFill #= FromFill0 - Amount,

    % Calculate new amount in To jug.
    ToFill #= ToFill0 + Amount 
  },
  % Create new list of three jug structures that represent new state.
  pours([
    jug(From, FromCapacity, FromFill),
    jug(To, ToCapacity, ToFill) | Other
  ]).

print_pour(from_to(F, T)) :-
  format("Pour from ~w to ~w.~n", [F, T]).

:- initialization((
   length(Pours, _), % for iterative deepening
   % Pours will be set to a list of from_to structures
   % that describe the shortest possible solution.
   phrase(pours([jug(a,4,0), jug(b,3,0), jug(c,7,7)]), Pours),
   maplist(print_pour, Pours),
   halt
 )).
