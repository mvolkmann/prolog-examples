% This demonstrates the three kinds of numbers supported in Prolog
% which are integer, floating point, and rational.

:- use_module(library(arithmetic)). % for rational_numerator_denominator
:- use_module(library(format)).

demo :-
  IntegerSum is 2 + 3,
  write(IntegerSum), nl, % 5

  FloatSum is 2.1 + 3.2,
  write(FloatSum), nl, % 5.300000000000001

  RationalSum is 1 rdiv 3 + 1 rdiv 6,
  write(RationalSum), nl. % 0.5

  % I can't get this to work.
  % rational_numerator_denominator(RationalSum, N, D),
  % format("N = ~w, D = ~w~n", [N, D]).
