% This is used to demonstrate foldl and foldr.
add(A, B, C) :- C is A + B.
mystery(X, A, B, C) :-
  format('mystery: X = ~w~n', [X]),
  format('mystery: B = ~w~n', [B]),
  C is A + B.

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

% Use this when the accumulator can start with the first element.
% The predicate will be passed the current accumulator value,
% a list element, and a variable to accept the new accumulator value.
foldl(Predicate, [H|T], Result) :-
    foldl(Predicate, T, H, Result).
foldl(_, [], Result, Result) :- !.
% Use this when a specific initial value for the accumulator must be specified.
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

% This creates a new list from an existing list by copying it
% and replacing the element at a given zero-based index with a new value.
% The first argument is the existing list.
% The second argument is the index of the element to be replaced.
% The third argument is the new value to be used at that index.
% The fourth argument is a variable to be set to the new list.
% For example, replace([a, b, c], 1, d, L) sets L to [a, d, c].
replace([], _, _, []).
replace([_|T], 0, Value, [Value|T]) :- !.
replace([H|T], Index, Value, [H|R]):-
  Index > 0,
  I is Index - 1,
  replace(T, I, Value, R).

% This gets the tail of a list that follows
% the last occurrence of a given element.
tail_after_last(_, [], []) :- !.
tail_after_last(E, L, A) :-
  member(E, L) ->
    [_|T] = L, tail_after_last(E, T, A);
    A = L.
