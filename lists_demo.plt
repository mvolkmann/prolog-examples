% To run these tests, enter `swipl lists.plt`

:- consult(lists_demo).
:- begin_tests(lists_demo).

test(list_append) :-
  list_append([], [], []),
  list_append([a], [], [a]),
  list_append([], [a], [a]),
  list_append([a, b], [c, d], [a, b, c, d]),
  !.

test(list_append_make_first) :-
  list_append(X, [c, d], [a, b, c, d]),
  X == [a, b],
  !.

test(list_append_make_second) :-
  list_append([a, b], X, [a, b, c, d]),
  X == [c, d],
  !.

test(list_append_make_third) :-
  list_append([a, b], [c, d], X),
  X == [a, b, c, d],
  !.

:- end_tests(lists_demo).
:- run_tests.
:- halt.
