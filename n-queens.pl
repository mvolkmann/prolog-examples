% See "N-Queens in Prolog" at https://www.youtube.com/watch?v=l_tbL9RjFdo.
:- use_module(library(clpfd)).

n_queens(N, Qs) :-
  length(Qs, N), % number of elements in list Qs must be N
  Qs ins 1..N, % all elements in Qs must be integers in range 1 to N
  safe_queens(Qs). % no elements in Qs can attack another

% If there are no queens in the list then none can be attacked.
safe_queens([]).

% Any queen that can attack another is also attacked by that same queen.
% So we only need to check queens to the right of the one being evaluated.
% All queens in a given list are safe if ...
safe_queens([Q|Qs]) :-

  safe_queens_(Q, Qs, 1),
  safe_queens(Qs).

safe_queens_(_, [], _).
safe_queens_(Q0, [Q|Qs], D) :-
  Q #\= Q0, % not same row
  abs(Q0 - Q) #\= D, % not same diagonal
  D2 #= D + 1,
  safe_queens_(Q0, Qs, D2).
  

:- initialization
  N = 20,

  /*
  % "Generate and Test" approach - very slow
  maplist(between(1, N), Qs),
  n_queens(N, Qs),
  */

  /*
  % "Early Prunning" approach - better
  n_queens(N, Qs), % much faster with this before next line
  maplist(between(1, N), Qs),
  */

  % "Intelligent Search" approach - much better
  n_queens(N, Qs),
  % The labeling predicate provides constraint propagation.
  % This avoids evaluating inconsistent solutions.
  % The first argument to labeling is a list of options
  % which can specify the search strategy to use.
  % The default search strategy is "leftmost"
  % which determines the element in Qs to evaluate next
  % based on which one is leftmost in the list.
  % "ff" is the "first fail" search strategy
  % which determines the element in Qs to evaluate next
  % based on the one with the fewest options.
  % "ffc" is the "most constrained" search strategy.
  % There are more predefined search strategies
  % and you can define custom strategies.
  labeling([ff], Qs),

  writeln(Qs),
  halt.
