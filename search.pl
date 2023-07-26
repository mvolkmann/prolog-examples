:- use_module(library(format)).
:- use_module(library(lists)).

edge(a, b).
edge(a, c).
edge(b, d).
edge(b, e).
edge(c, f).
edge(c, g).
edge(g, h).
edge(h, i).

/*
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

from_to_path(From, To, Path) :- from_to_path_(From, To, Path).

from_to_path_(Node, Node, [Node]).

from_to_path_(From, To, Path) :-
  format("searching for path from ~w to ~w~n", [From, To]),
  edge(ToParent, To),
  from_to_path_(From, ToParent, Path1),
  \+ member(To, Path1), % To is NOT a member of Path1; avoids cycles
  append(Path1, [To], Path).

:- initialization((
  /*
  % length(Path, _),
  % format("length is ~d~n", L)
  */
  from_to_path(a, i, Path),
  format("path is ~w~n", [Path])
)).

