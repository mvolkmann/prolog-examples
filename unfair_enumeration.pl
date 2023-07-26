:- use_module(library(lists)).

as([a]).
as([a|T]) :- as(T).

bs([b]).
bs([b|T]) :- bs(T).

as_and_bs(X) :- as(A), bs(B), append(A, B, X).

% To generate solutions using iterative deepening, enter the following:
% length(X, _), as_and_bs(X).
% Why does this run forever and never generates any solutions?
