/*
% These are only needed in Scryer Prolog.
:- use_module(library(clpz)). % Scryer Prolog
:- use_module(library(dcgs)). % Only needed in Scryer Prolog
:- use_module(library(lists)). % Only needed in Scryer Prolog
*/

% This is needed in SWI-Prolog.
:- use_module(library(clpfd)).

% These facts specify the capacity of each jug.
jug_capacity(a, 8).
jug_capacity(b, 5).
jug_capacity(c, 3).

% This specifies the goal state which is to have
% 4 in jugs a and b, and 0 in jug c.
moves(Jugs) --> {
  member(jug(a, 4), Jugs),
  member(jug(b, 4), Jugs)
}.

% What makes this do a breadth first search?
% How does this avoid evaluating duplicate states?
% Can this approach be used in rush_hour.pl?
% The --> operator and the phrase predicate are part of DCG.
% Jugs0 is a list of jug structures.
moves(Jugs0) -->
  % A list of from_to structures will be created that describe the solution.
  [from_to(From, To)],
  {
    format('* Evaluating moving from ~w to ~w.~n', [From, To]),

    % This creates the list Jugs1 from
    % all the elements in Jugs0 except the From jug.
    % It also sets FromFill0 to the current units in the From jug.
    select(jug(From, FromFill0), Jugs0, Jugs1),
    format('Jugs1 = ~w~n', [Jugs1]),
    format('FromFill0 = ~w~n', [FromFill0]),

    % The from jug cannot be empty.
    FromFill0 #> 0,

    % This creates the list Jugs from
    % all the elements in Jugs1 except the To jug.
    % It also sets ToFill0 to the current units in the To jug.
    select(jug(To, ToFill0), Jugs1, Jugs),
    format('Jugs = ~w~n', [Jugs]),
    format('ToFill0 = ~w~n', [ToFill0]),

    % This gets the capacity of the To jug when it is empty.
    jug_capacity(To, ToCapacity),
    format('ToCapacity = ~w~n', [ToCapacity]),

    % The To jug cannot be full.
    ToFill0 #< ToCapacity,

    % This gets the amount that can be moved from the From jug to the To jug.
    Move #= min(FromFill0, ToCapacity-ToFill0),
    format('* Move ~w from ~w to ~w~n', [Move, From, To]),

    % This removes Move units from the From jug.
    FromFill #= FromFill0 - Move,

    % This adds Move units to the To jug.
    ToFill #= ToFill0 + Move
  },
  moves([jug(From,FromFill), jug(To,ToFill)|Jugs]).

:- initialization
  % phrase is part of DCG.
  InitialState = [jug(a, 8), jug(b, 0), jug(c, 0)],
  length(Ms, _), phrase(moves(InitialState), Ms),
  maplist(writeln, Ms),
  halt.
/*
One solutions is:
Ms = [
  from_to(a,b),
  from_to(b,c),
  from_to(c,a),
  from_to(b,c),
  from_to(a,b),
  from_to(b,c),
  from_to(c,a)
]
*/
