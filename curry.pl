:- use_module(library(clpfd)).

sum2(X, Y, Z) :-
  Z #= X + Y.

sum3(A1, A2, A3, A4) :-
  A4 #= A1 + A2 + A3.

:- initialization
  sum3(1, 2, 3, R1),
  writeln(R1), % 6

  sum3(1, R2, 3, 6),
  writeln(R2), % 2

  % Currying one argument.
  call(sum3(10), 20, 30, R3),
  writeln(R3), % 60

  % Currying two arguments.
  call(sum3(10, 20), 30, R4),
  writeln(R4), % 60

  % Currying goal passed to maplist.
  Numbers = [1, 2, 3],
  maplist(sum2(10), Numbers, R5),
  writeln(R5), % [11,12,13]

  P = <, % could be set to a different relational operator
  Term =.. [P, 3, 5], % builds term from list containing functor and arguments
  % call(Term), % evaluates term
  (call(Term) -> writeln('yes'); writeln('no')), % yes

  halt.
