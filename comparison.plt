% To run these tests, enter `swipl comparison.plt`

:- begin_tests(comparison).

% Suppress warnings about unused variables.
:- style_check(-singleton).

test(equal) :-
  X is 1,
  Y is 1,
  X =:= Y.

test(not_equal) :-
  X is 1,
  Y is 2,
  X =\= Y.

test(identical) :-
  x(A, B) == x(A, B).

test(not_identical) :-
  x(A, B) \== x(C, D).

test(structurally_equivalent) :- 
  x(A, B) =@= x(C, D).

test(not_structurally_equivalent) :- 
  x(A, B) \=@= x(C, D, E),
  x(A, B) \=@= y(C, D).

:- end_tests(comparison).
:- run_tests.
:- halt.
