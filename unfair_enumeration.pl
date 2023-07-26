:- use_module(library(lists)).

as([a]).
as([a|T]) :- as(T).

bs([b]).
bs([b|T]) :- bs(T).

as_and_bs(X) :- as(A), bs(B), append(A, B, X).
