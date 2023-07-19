calc(Atom) :-
  % Convert an atom to a Prolog term.
  term_to_atom(Term, Atom),
  % Assume the term is an arithmetic expression and evaluate it.
  A is Term,
  % Write the result to stdout.
  writeln(A).

