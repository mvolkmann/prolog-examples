:- use_module(library(reif)).

% Greater or equal
ge(=, true).
ge(>, true).
ge(<, false).

has_length(List, Bool) :-
  length(List, Length),
  zcompare(Compare, Length, 6),
  ge(Compare, Bool).

demo :-
  L0 = ["apple", "banana", "cherry", "date"],
  tfilter(has_length, L0, L1),
  write(L1), nl. % ["banana", "cherry"]

