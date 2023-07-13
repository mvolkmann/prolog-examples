% This is used to demonstrate foldl and foldr.
add(A, B, C) :- C is A + B.

% Gets a column of values from a 2D list.
column(_, [], []) :- !.
column(N, Board, Column) :-
  [Row|Rest] = Board,
  nth0(N, Row, Element),
  column(N, Rest, Column2),
  Column = [Element | Column2].

% This creates a list containing N copies of E.
fill(0, _, []) :- !.
fill(N, E, L) :-
  N2 is N - 1,
  fill(N2, E, L2),
  L = [E | L2].

% See https://pbrown.me/blog/functional-prolog-map-filter-and-reduce/.
foldl(Predicate, [H|T], Result) :-
    foldl(Predicate, T, H, Result).
foldl(_, [], Result, Result) :- !.
foldl(Predicate, [H|T], Acc, Result) :-
    call(Predicate, Acc, H, NewAcc),
    foldl(Predicate, T, NewAcc, Result).

foldr(_, [LastElem|[]], LastElem) :- !.
foldr(Predicate, [H|T], Result) :-
    foldr(Predicate, T, Acc),
    call(Predicate, H, Acc, Result).

% These are helper rules for repeat below.
repeat_(_, 0, []) :- !.
repeat_(Char, N, [Char|T]) :-
  N2 is N - 1,
  repeat_(Char, N2, T).

% This creates a string containing a given character repeated N times.
% The first two arguments must be instantiated (ground).
repeat(Char, N, S) :-
  ground(Char),
  ground(N),
  repeat_(Char, N, L),
  atomics_to_string(L, S).

% This replaces a list element at a given zero-based index.
% For example, replace([a, b, c], 1, d, L) sets L to [a, d, c].
replace([], _, _, []).
replace([_|T], 0, X, [X|T]) :- !.
replace([H|T], I, X, [H|R]):- I > 0, I1 is I-1, replace(T, I1, X, R).

% This gets the tail of a list that follows
% the last occurrence of a given element.
tail_after_last(_, [], []) :- !.
tail_after_last(E, L, A) :-
  member(E, L) ->
    [_|T] = L, tail_after_last(E, T, A);
    A = L.
