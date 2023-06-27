% To run these tests, enter `swipl geometry.plt`

:- consult(geometry).
:- begin_tests(geometry).

near(A, B) :- abs(A - B) =< 1e-7.

test(area) :-
  area(circle, 3, X), near(X, 28.27433388231), !.

test(area) :-
  area(circle, X, 10), near(X, 1.784124116153), !.

test(compute_circle_area) :-
  radius_area(3, X), near(X, 28.27433388231), !.

test(compute_circle_radius) :-
  radius_area(X, 10), near(X, 1.784124116153), !.

:- end_tests(geometry).
:- run_tests.
:- halt.
