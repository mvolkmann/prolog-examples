female(amanda).
female(tami).
male(clarence).
male(jeremy).
male(mark).
male(richard).
father(clarence, tami).
father(richard, mark).
father(mark, amanda).
father(mark, jeremy).
mother(tami, amanda).
mother(tami, jeremy).

grandfather(X, Y) :-
  male(X),
  father(X, Z),
  (father(Z, Y); mother(Z, Y)).
