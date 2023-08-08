:- use_module(library(reif)).

check(P, E, B) :-
  Goal =.. [P, E],
  ( Goal -> B = true; B = false).

list_matching(L0, P, L) :- tfilter(check(P), L0, L).

% has_length(Len, S) :- length(S, L), L >= Len.

is_long([_,_,_,_,_]).

demo :-
  L0 = ["apple", "banana", "cherry", "date"],
  % list_matching(L0, has_length(5), L),
  list_matching(L0, is_long, L),
  write(L), nl. % ["banana", "cherry"]
