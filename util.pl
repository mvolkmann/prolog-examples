% Some of these predicates are also defined in the "apply" library,
% but its foldl predicate requires specifying the initial value
% of the accumulator rather than using the first list element.

:- use_module(library(clpfd)).

% This is used to demonstrate foldl and foldr.
sum2(A, B, C) :- C #= A + B.
sum3(P, A, B, C) :-
  C #= P + A + B.

% Gets a column of values from a 2D list.
% TODO: How can you avoid using cut AND
% TODO: make this terminate without a choicepoint?
column(_, [], []) :- !.
column(N, Board, [Element | Column2]) :-
  [Row|Rest] = Board,
  nth0(N, Row, Element),
  column(N, Rest, Column2).

% This gets the tail of a list that follows
% the last occurrence of a given element.
tail_after_last(_, [], []) :- !.
tail_after_last(E, L, A) :-
  ( member(E, L) ->
    [_|T] = L,
    tail_after_last(E, T, A)
  ; A = L
  ).
