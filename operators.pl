:- arithmetic_function(dbl/1).
:- op(10, fx, dbl).
dbl(X, Y) :- Y is X * 2.
% ?- X is dbl 5. % gives 10

