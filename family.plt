% To run these tests, enter `swipl family.plt`
:- consult(family).
:- begin_tests(family).

test(grandfather) :-
  grandfather(clarence, jeremy),
  grandfather(clarence, amanda),
  grandfather(richard, jeremy),
  grandfather(richard, amanda),
  !.

test(is_father) :-
  is_father(clarence),
  is_father(richard),
  is_father(mark),
  !.

test(is_mother) :-
  is_mother(gerri),
  is_mother(judi),
  is_mother(tami),
  !.

test(is_son) :-
  is_son(jeremy),
  is_son(mark),
  !.

test(sibling) :-
  sibling(amanda, jeremy).

test(sister) :-
  sister(amanda, jeremy),
  \+ sister(amanda, amanda).

:- end_tests(family).
:- run_tests.
:- halt.
