:- use_module(library(clpz)). % for ins, #=, and label
:- use_module(library(format)).

demo :-
  % Find all combinations of integers >= 0 whose sum is 3.
  [A, B, C] ins 0..sup,
  A + B + C #= 3,
  label([A, B, C]),
  format("~d + ~d + ~d = 3~n", [A, B, C]).

demo2 :-
  % Find all combinations of integers between -6 and 6 whose product is -6.
  % This does not currently work as expected.
  % Dr. Triska submitted a PR to fix this.
  % See https://github.com/mthom/scryer-prolog/pull/1938
  [A, B, C] ins -6..6,
  A * B * C #= -6,
  label([A, B, C]),
  format("~d * ~d * ~d = -6~n", [A, B, C]).
