:- use_module(library(debug)).

writeln(X) :- write(X), nl.

anger(Level) :-
  ( Level < 10 ->
    writeln(red)
  ; throw(error(
      domain_error(too_angry, Level),
      anger/1
    ))
  ).

envy :-
  writeln(green).

demo :-
  $- anger(9),
  $ envy,
  * writeln(blue).
  
