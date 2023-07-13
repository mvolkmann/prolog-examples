male(mark).

check(Name) :-
  writeln('before arrow'),
  (male(Name) ->
    format('~w is male.~n', [Name]);
    format('~w is not male.~n', [Name])
  ),
  writeln('after arrow').

:- initialization

  check(mark),
  check(jeremy),
  halt.
