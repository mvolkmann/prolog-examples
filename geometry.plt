% To run these tests, enter `swipl geometry.plt`

:- consult(geometry).
:- begin_tests(geometry).

near(A, B) :- abs(A - B) =< 1e-7.

test(double) :-
  double(3, X), X =:= 6.

test(area) :-
  area(circle, 3, X), near(X, 28.27433388231), !.

test(area) :-
  area(circle, X, 10), near(X, 1.784124116153), !.

:- end_tests(geometry).
:- run_tests.
:- halt.
