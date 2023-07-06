:- arithmetic_function(dbl/1).
:- op(10, fx, dbl).
dbl(X, Y) :- ground(X), Y is X * 2.
dbl(X, Y) :- ground(Y), X is Y / 2.

:- initialization
  dbl(5, X),
  writeln(X), % 10
  dbl(Y, 10),
  writeln(Y), % 5
  halt.
