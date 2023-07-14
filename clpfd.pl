:- use_module(library(clpfd)).

add(A, B, C) :- C #= A + B.

:- initialization
  X in 5..10, Y in 7..14, X #> Y,
  label([X, Y]),

  add(2, 3, R2),
  format('R2 = ~w~n', [R2]), % 5

  add(R3, 3, 5),
  format('R3 = ~w~n', [R3]), % 2

  add(2, R4, 5),
  format('R4 = ~w~n', [R4]), % 3

  findall(
    [A, B], % transform each result into the list [A, B]
    % Find all pairs of integers that satisfy these constraints.
    (add(A, B, 5), A in 1..5, A #> B, label([A, B])),
    Results % sets this
  ),
  format('Results = ~w~n', [Results]), % [[3,2],[4,1],[5,0]]

  halt.
