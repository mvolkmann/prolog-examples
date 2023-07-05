:- set_prolog_flag(double_quotes, chars).

xs --> "".
xs --> "x", xs.

% phrase(xs, Ls). % finds all possible solutions

% The following predicates are DCG building blocks.

% Definition of a sequence.
seq([]) --> [].
seq([H|T]) --> [H], seq(T).

append(Xs, Ys, Zs) :-
  phrase((seq(Xs), seq(Ys)), Zs).
  % :- append("abc", "xyz", L), writeln(L). % output is [a,b,c,x,y,z]

% Concatentation of any number of lists.
seqq([]) --> [].
seqq([H|T]) --> seq(H), seqq(T).

:- phrase(seqq(["ab", "cd", "ef"]), L), writeln(L). % output is [a,b,c,d,e,f].

% qes is the reverse of seq.
qes([]) --> [].
qes([H|T]) --> qes(T), [H].

panindrome(L) :- phrase(qes(L), L).

% This describes any sequence.
... --> [] | [_], ... .
% It can be used to get the last element in a list.
% ?- phrase((..., [Last]), "xyz"). % output is Last = z; false.
