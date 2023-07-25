% This matches lists like [x, x, x].
% It does not match `xxx`.
xs --> [].
xs --> [x], xs.

% This matches lists like [121, 121, 121].
% It also matches `yyy`.
ys --> "".
ys --> "y", ys.

% phrase(xs, Ls). % finds all possible solutions

% The following predicates are DCG building blocks.

% Definition of a sequence.
% Scryer Prolog provides this in its dcgs library.
seq([]) --> [].
seq([H|T]) --> [H], seq(T).

% Concatenation of two lists.
append(Xs, Ys, Zs) :- phrase((seq(Xs), seq(Ys)), Zs).
% :- append("abc", "xyz", L), writeln(L). % output is [a,b,c,x,y,z]

% Concatenation of any number of lists.
% Scryer Prolog provides this in its dcgs library.
seqq([]) --> [].
seqq([H|T]) --> seq(H), seqq(T).
% :- phrase(seqq(["ab", "cd", "ef"]), L), writeln(L). % output is [a,b,c,d,e,f].

% qes is the reverse of seq.
qes([]) --> [].
qes([H|T]) --> qes(T), [H].

panindrome(L) :- phrase(qes(L), L).
% palindrome("mother"). % false
% palindrome("mom"). % true

% This describes any sequence.
% Scryer Prolog provides this in its dcgs library.
... --> [] | [_], ... .

% ... can be used to get the last element in a list.
% ?- phrase((..., [Last]), `xyz`). % output is Last = 122 (code for z); false.

% ... can be used to determine if a given sublist
% occurs anywhere in a list.
% ?- phrase((..., "y", ...), `xyz`). % output is true
% ?- phrase((..., "ar", ...), `Mark`). % output is true

% ... can be used to determine if
% any element occurs twice in succession in a list.
% ?- phrase((..., [X, X], ...), `Mississippi`).
% finds 115 (s), 115 (s), and 112 (p)

% This represents trees as a DCG.
nodes(nil) --> [].
nodes(node(Node, L, R)) --> [Node], nodes(L), nodes(R).
/*
?- phrase(
  nodes(
    node(
      a,
      nil,
      node(
        b,
        node(c, nil, nil),
        nil
      )
    )
  ),
  L
). % output is a list of the leaf node values [a, b, c].
*/
