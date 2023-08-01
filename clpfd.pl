% for SWI-Prolog
% :- use_module(library(clpfd)).

% for Scryer Prolog
:- use_module(library(clpz)).
:- use_module(library(format)).

add1(A, B, C) :- C is A + B.
add2(A, B, C) :- C #= A + B.

print(X, Y) :- format("X = ~w, Y = ~w~n", [X, Y]).

constraints_demo :-
  findall(
    [X, Y],
    (X in 5..10, Y in 7..14, X #> Y, label([X, Y])),
    Results
  ),
  write(Results),
  nl.

card_demo1 :-
  Vs = [2, 4, 2, 3, 2, 4],
  global_cardinality(Vs, [2-3, 3-1, 4-2]).

card_demo2 :-
  global_cardinality(Vs, [2-3, 3-1, 4-2]).

sup_demo :-
  findall(
    [Diff],
    (X in 20..sup, Y in 30..40, Diff #= Y - X, label([Diff])),
    Results
  ),
  write(Results),
  nl.
