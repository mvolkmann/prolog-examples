% These are facts that say comet is a whippet and spots is a cheetah.
whippet(george).
whippet(comet).
cheetah(spots).

% These are rules that say if something is
% a whippet or cheetah then it is fast.
% fast(X) :- whippet(X).
% fast(X) :- cheetah(X).
fast(X) :- cheetah(X); whippet(X).

