:- use_module(library(clpz)). % for zcompare
:- use_module(library(reif)). % for tfilter

% Greater or equal
ge(=, true).
ge(>, true).
ge(<, false).

length_at_least(Length, List, Bool) :-
  length(List, Len),
  zcompare(Compare, Len, Length), % sets Compare to =, >, or <
  ge(Compare, Bool). % sets Bool to true or false

demo :-
  L0 = ["apple", "banana", "cherry", "date"],
  tfilter(length_at_least(6), L0, L),
  L == ["banana", "cherry"]. % succeeds

