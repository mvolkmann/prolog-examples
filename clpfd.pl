% for SWI-Prolog
% :- use_module(library(clpfd)).

% for Scryer Prolog
:- use_module(library(clpz)).
:- use_module(library(format)).

add1(A, B, C) :- C is A + B.
add2(A, B, C) :- C #= A + B.

print(X, Y) :- format("X = ~w, Y = ~w~n", [X, Y]).

:- initialization((
  findall(
    [X, Y],
    (X in 5..10, Y in 7..14, X #> Y, label([X, Y])),
    Results
  ),
  write(Results),
  nl

  % halt
)).
