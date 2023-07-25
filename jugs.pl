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
% The "moves" grammar rule is satisfied if Jugs
% satisfies the Prolog predicates in curly braces.
% This acts as a terminating rule, preventing
% the next moves rule from recursing indefinitely.
moves(Jugs) --> {
  member(jug(a, 4), Jugs),
  member(jug(b, 4), Jugs)
  % It is not technically necessary to specify that jug c is empty
  % because the total of all jugs is always 8.
  % But doing so doesn't hurt.
  % member(jug(c, 0), Jugs)
}.

% The use of iterative deepening provided by specifying `length(Ms, _)` below
% is what makes this do a breadth first search
% and avoid evaluating duplicate states.
% Can this approach be used in rush_hour.pl?
% The --> operator and the phrase predicate are part of DCG.
% Jugs0 is a list of jug structures.
moves(Jugs0) -->
  % This represents the next single move in
  % the list of moves to reach the goal state.
  [from_to(From, To)],

  % This executes some Prolog predicates that
  % set variables used by the grammar rule.
  {
    % Jugs0 is a list of all three jugs.
    % This creates the 2-element list Jugs1
    % from the list Jugs0 excluding the From jug.
    % It also sets FromFill0 to the current units in the From jug.
    % After this we can try moving units
    % from the From jug to each of the two jugs held in Jugs1.
    select(jug(From, FromFill0), Jugs0, Jugs1),
    %format('Jugs1 = ~w~n', [Jugs1]),

    % The from jug cannot be empty.
    FromFill0 #> 0,

    % This creates the list Jugs which will contain
    % only one of the jugs in Jugs1.
    % This sets To to the name of the jug in Jugs1
    % that does not match the name of the jug in the list Jugs.
    % It also sets ToFill0 to the current units in the To jug.
    select(jug(To, ToFill0), Jugs1, Jugs),
    %format('Jugs = ~w~n', [Jugs]),
    %format('To = ~w~n', [To]),

    % This gets the capacity of the To jug when it is empty.
    jug_capacity(To, ToCapacity),

    % The To jug cannot be full.
    ToFill0 #< ToCapacity,

    % This gets the amount that can be moved from the From jug to the To jug.
    Move #= min(FromFill0, ToCapacity-ToFill0),
    %format('Move ~w from ~w to ~w~n', [Move, From, To]),

    % This removes Move units from the From jug.
    FromFill #= FromFill0 - Move,

    % This adds Move units to the To jug.
    ToFill #= ToFill0 + Move
  },

  % This generates a list of the remaining moves
  % required to reach the goal state.
  moves([jug(From, FromFill), jug(To, ToFill) | Jugs]).

:- initialization((
  InitialState = [jug(a, 8), jug(b, 0), jug(c, 0)],
  % This causes iterative deepening to be used.
  length(Ms, _),
  % The phrase predicate is defined by the DCG library.
  phrase(moves(InitialState), Ms),
  maplist(writeln, Ms),
  halt
)).
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
