% FACTS

female(amanda).
female(gerri).
female(judi).
female(tami).

male(clarence).
male(jeremy).
male(mark).
male(richard).

father(richard, mark).
father(clarence, tami).
father(mark, amanda).
father(mark, jeremy).

mother(judi, richard).
mother(gerri, tami).
mother(tami, amanda).
mother(tami, jeremy).

% RULES

grandfather_of(X, Y) :-
  father(X, P),
  (father(P, Y);mother(P, Y)),
  !.

is_father(X) :- father(X, _), !.

is_mother(X) :- mother(X, _), !.

is_son(X) :-
  (father(_, X); mother(_, X)), !.

sibling(X, Y) :-
  father(F, X),
  father(F, Y),
  mother(M, X),
  mother(M, Y).

sister_of(X, Y) :-
  \+ X = Y, % can't be sister of self
  female(X),
  sibling(X, Y).
