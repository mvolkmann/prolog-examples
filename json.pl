:- use_module(library(serialization/json)).

demo :-
  V = [foo, bar],
  phrase(json:json_chars(V), J),
  write(J), nl.
