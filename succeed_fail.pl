good(X) :-
  writeln(X),
  true.

bad(X) :-
  writeln(X),
  false.

worse(X) :-
  writeln(X),
  fail.
