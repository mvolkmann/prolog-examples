% To run these tests, enter `swipl exercise1_3.plt`

:- consult(exercise1_3).
:- begin_tests(exercise1_3).

test(grandfather_of) :-
  grandfather_of(clarence, jeremy),
  grandfather_of(clarence, amanda),
  grandfather_of(richard, jeremy),
  grandfather_of(richard, amanda).

test(is_father) :-
  is_father(clarence),
  is_father(richard),
  is_father(mark).

test(is_mother) :-
  is_mother(gerri),
  is_mother(judi),
  is_mother(tami).

test(is_son) :-
  is_son(jeremy),
  is_son(mark).

test(sibling) :-
  sibling(amanda, jeremy).

test(sister_of) :-
  sister_of(amanda, jeremy),
  \+ sister_of(amanda, amanda). % tests exercise 1.4

:- end_tests(exercise1_3).
:- run_tests.
:- halt.
