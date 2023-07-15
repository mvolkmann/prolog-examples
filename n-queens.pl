/*
See "N-Queens in Prolog" at https://www.youtube.com/watch?v=l_tbL9RjFdo.
This code can test, complete, and generate solutions.  For example:

The following tests whether a given solution is valid:
  `n_queens(4, [2, 4, 1, 3]).`

The following completes the solution beginning with 2 and 4:
  `n_queens(4, [2, 4, X, Y]).`

The following generates all possible solutions for a board size of 4:
  `n_queens(4, L), labeling(L).`
*/

:- use_module(library(clpfd)).
/* For Scryer Prolog animation ...
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(dcgs)).
:- use_module(library(freeze)).
:- use_module(library(charsio)).
*/

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
  % Queen Q is safe from the first queen in Qs.
  safe_queens_(Q, Qs, 1),
  % All the queens in Qs are safe from each other.
  safe_queens(Qs).

% Any queen is safe from queens to its right
% if there are no queens to its right.
safe_queens_(_, [], _).

% Distance is the difference in columns
% between Q0 and the column to be evaluated.
% A queen Q0 is safe from queens to its right if ...
safe_queens_(Q0, [Q|Qs], Distance) :-
  Q0 #\= Q, % it is not in the same row
  abs(Q - Q0) #\= Distance, % it is not on the same diagonal
  % Check the next column to the right.
  NextDistance #= Distance + 1,
  safe_queens_(Q0, Qs, NextDistance).

/* For Scryer Prolog animation ...
postscript -->
  "/init { /N exch def 340 N div dup scale -1 -1 translate \
           /Palatino-Roman 0.8 selectfont 0 setlinewidth \
           1 1 N { 1 1 N { 1 index c } for pop } for } bind def \
   /r { translate 0 0 1 1 4 copy rectfill 0 setgray rectstroke } bind def \
   /i { gsave 0.5 setgray r grestore } bind def \
   /q { gsave r 0.5 0.28 translate (Q) dup stringwidth pop -2 div 0 moveto \
        1 setgray show grestore } bind def \
   /c { gsave 1 setgray r grestore } bind def\n".

show(N, Options) :-
  N #> 0,
  n_queens(N, Qs),
  phrase(postscript, Ps),
  format("~s ~w init\n", [Ps,N]),
  animate(Qs),
  labeling(Options, Qs),
  get_single_char(_),
  false.
show(_, _ ) :- halt.
*/

:- initialization
  N = 20,

  /*
  % "Generate and Test" approach - very slow;
  % 17 seconds with N = 8; seemingly forever with N = 20 
  length(Qs, N),
  % Generate every possible combination of queen positions.
  maplist(between(1, N), Qs),
  % Determine which of the combinations are solutions.
  n_queens(N, Qs),
  */

  /*
  % "Early Prunning" approach - better; 4.32 seconds with N = 20
  % The length predicate is not needed with this approach
  % because it is enforced in the "n_queens" predicate.
  n_queens(N, Qs), % much faster with this before next line
  maplist(between(1, N), Qs),
  */

  % "Intelligent Search" approach - much better; 0.11 seconds with N = 20
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
  % phrase(format('Qs = ~w~n', [Qs])).
  halt.
