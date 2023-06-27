appendStrings(S1, S2, SR) :-
  name(S1, L1),
  name(S2, L2),
  append(L1, L2, LR),
  name(SR, LR).

% Example usage:
% appendStrings('first ', 'second', X).
% X = 'first second'
