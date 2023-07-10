appendStrings(S1, S2, SR) :-
  name(S1, L1),
  name(S2, L2),
  append(L1, L2, LR),
  name(SR, LR).

% Example usage:
% appendStrings('first ', 'second', X).
% X = 'first second'

repeat_(_, 0, []) :- !.
repeat_(Char, N, [Char|T]) :-
  N2 is N - 1,
  repeat_(Char, N2, T).
% repeat("*", 3, S). 

% The first two arguments must be instantiated (ground).
repeat(Char, N, S) :-
  ground(Char),
  ground(N),
  repeat_(Char, N, L),
  atomics_to_string(L, S).
