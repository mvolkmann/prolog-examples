% Also see reif.pl!
:- use_module(library(reif)).

dog(comet).

writeln(X) :- write(X), nl.

report(Name) :-
  ( dog(Name) ->
    writeln(dog)
  ; writeln('not a dog')
  ).

is_dog(X, true) :- dog(X).
is_dog(X, false) :- \+ dog(X).

report_reif(Name) :-
  % The first argument must be a predicate that accepts
  % an extra variable argument to receive true or false.
  if_(
    is_dog(Name),
    writeln(dog),
    writeln('not a dog')
  ).

demo :-
  report(comet), % dog
  report(mark), % not a dog
  report_reif(comet), % dog
  report_reif(mark). % not a dog
