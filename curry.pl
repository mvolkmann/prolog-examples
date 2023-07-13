sum4(A1, A2, A3, A4) :-
  A4 is A1 + A2 + A3.

mystery(X, Y, Z) :-
  Z is X + Y.

:- initialization
  sum4(1, 2, 3, R1),
  writeln(R1), % 6

  % Currying one argument.
  call(sum4(10), 20, 30, R2),
  writeln(R2), % 60

  % Currying two arguments.
  call(sum4(10, 20), 30, R3),
  writeln(R3), % 60

  % Currying goal passed to maplist.
  Numbers = [1, 2, 3],
  maplist(mystery(10), Numbers, R4),
  writeln(R4), % [11,12,13]

  P = <, % could be set to a different relational operator
  Term =.. [P, 3, 5], % builds term from list containing functor and arguments
  % call(Term), % evaluates term
  call(Term) -> writeln('yes'); writeln('no'),

  halt.
