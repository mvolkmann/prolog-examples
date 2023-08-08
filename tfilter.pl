:- use_module(library(reif)).
:- use_module('lib/list_util').

has_length(List, Length) :-
  length(List, Len),
  Len >= Length.

is_long(List, Bool) :-
  length(List, Len),
  goal_bool(Len > 5, Bool).
  % This doesn't work as an alternative to the previous two lines.
  % goal_bool(has_length(List, 6), Bool).

demo :-
  L0 = ["apple", "banana", "cherry", "date"],
  tfilter(is_long, L0, L1),
  write(L1), nl. % ["banana", "cherry"]

