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

test(alphabetically) :-
  'dog' @< 'fox',
  'fox' @> 'dog',
  'dog' == 'dog',
  'dog' @=< 'dog',
  'dog' @>= 'dog'.

test(identical) :-
  x(A, B) == x(A, B). % same functor name and argument variables

test(not_identical) :-
  x(A, B) \== x(C, D). % different argument variables

test(structurally_equivalent) :- 
  x(A, B) =@= x(C, D).

test(not_structurally_equivalent) :- 
  x(A, B) \=@= x(C, D, E), % different arity
  x(A, B) \=@= y(C, D). % different functor name

:- end_tests(comparison).
:- run_tests.
:- halt.
