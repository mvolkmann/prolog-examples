:- use_module(library(dcgs)).
:- use_module(library(lists)).

as --> "a", as_.
as_ --> [] | as.

bs --> "b", bs_.
bs_ --> [] | bs.

as_and_bs --> as, bs.

% To generate solutions using iterative deepening, enter the following:
% length(X, _), phrase(as_and_bs, X).
