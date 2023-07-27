% These lines are needed in Scryer Prolog, but not in SWI-Prolog.
:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(format)).
:- use_module(library(lists)).

/* This is an alternate strategy.
from_to_path(From, To, Path) :-
  from_to_path_(From, To, P),
  append([From], P, Path).

from_to_path_(From, To, [To]) :-
  edge(From, To),
  format("evaluated ~w~n", [From]).

from_to_path_(From, To, [Node|Path]) :-
  format("evaluated ~w~n", [From]),
  edge(From, Node),
  from_to_path_(Node, To, Path).
*/

/* This is an alternate strategy.
from_to_path(From, To, Path) :- from_to_path_(From, To, Path).

from_to_path_(Node, Node, [Node]).

% This searches backwards from To back to From.
from_to_path_(From, To, Path) :-
  format("searching for path from ~w to ~w~n", [From, To]),
  edge(ToParent, To),
  from_to_path_(From, ToParent, Path1),
  \+ member(To, Path1), % To is NOT a member of Path1; avoids cycles
  append(Path1, [To], Path).
*/

edge(a, b).
edge(a, c).
edge(b, d).
edge(b, e).
edge(c, f).
edge(c, g).
edge(d, h).
edge(d, i).
edge(e, j).
edge(e, k).

from_to_path(From, To, Path) :-
  phrase(from_to_path_(From, To, []), Path).

from_to_path_(To, To, _) -->
  { write('found solution'), nl},
  [To]. % adding a cut here does not help

from_to_path_(From, To, PreviousPath) -->
  {
    format("evaluating ~w~n", [From]),
    edge(From, Next),
    maplist(dif(To), PreviousPath) % include to avoid cycles
  },
  [From],
  from_to_path_(Next, To, [From|PreviousPath]).

run :-
  length(Path, L), % for iterative deepening
  format("path length is ~d~n", [L]), % 3
  % Wrapping the next goal in once does not help.
  from_to_path(a, g, Path),
  % This searches:
  % a
  % a b c
  % a b d e c f
  % The solution [a,c,g] is found.
  % Then searching continues.
  % a b d h i e j k c f
  % How can I stop it from continuing to search after a solution is found?
  format("path is ~w~n", [Path]). % [a,c,g]

