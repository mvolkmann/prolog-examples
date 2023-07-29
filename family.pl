female(amanda).
female(gerri).
female(judi).
female(tami).

male(clarence).
male(jeremy).
male(mark).
male(richard).

father(clarence, tami).
father(richard, mark).
father(mark, amanda).
father(mark, jeremy).

mother(judi, mark).
mother(gerri, tami).
mother(tami, amanda).
mother(tami, jeremy).

grandfather(X, Y) :-
  male(X),
  father(X, Z),
  (father(Z, Y); mother(Z, Y)).

is_father(X) :- father(X, _).

is_mother(X) :- mother(X, _).

is_son(X) :-
  male(X), (father(_, X); mother(_, X)).

sibling(X, Y) :-
  \+ X = Y, % can't be sibling of self
  father(F, X),
  father(F, Y),
  mother(M, X),
  mother(M, Y).

sister(X, Y) :-
  \+ X = Y, % can't be sister of self
  female(X),
  sibling(X, Y).
