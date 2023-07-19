calc(Atom) :-
  % Convert an atom to a Prolog term.
  term_to_atom(Term, Atom),
  % Assume the term is an arithmetic expression and evaluate it.
  A is Term,
  % Write the result to stdout.
  writeln(A).

% The remaining clauses were copied from family.pl.

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
