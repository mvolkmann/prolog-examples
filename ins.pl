:- use_module(library(clpz)). % for ins, #=, and label

% Enter demo1(T) to test this.
demo1(A + B + C = 3) :-
  % Find all combinations of integers >= 0 whose sum is 3.
  Vs = [A, B, C],
  Vs ins 0..sup,
  A + B + C #= 3,
  label(Vs).

% Enter demo2(T) to test this.
demo2(A * B * C = -6) :-
  % Find all combinations of integers between -6 and 6 whose product is -6.
  Vs = [A, B, C],
  Vs ins -6..6,
  A * B * C #= -6,
  label(Vs).
