% Uncomment the following line to use Scryer Prolog.
% :- use_module(library(clpz)).

double(X, R) :- R #= X * 2.

/* TODO: Fix these.
isEven(N) :- mod(N, 2) =:= 0.
isEven(N, R) :- R is mod(N, 2) = 0.
isOdd(N) :- mod(N, 2) =:= 1.
isOdd(N, R) :- R is mod(N, 2) = 1.
*/

print_list_parts([H|T]) :-
  format("head is ~w, tail is ~w", [H, T]).

print_elements([]).

print_elements([H|T]) :-
  writeln(H),
  print_elements(T).

print_second(L) :-
  [_, S] = L,
  write(S), nl.

/*
% When the input list is empty, the output list should be empty.
maplist(_, [], []).

% This is a reimplementation of the "maplist" rule in the "lists" library.
% Enter a query like `maplist(double, [1, 2, 3], X).`
% The result should be `X = [2, 4, 6]`.
% [H1|T1] represents the input list.
% [H2|T2] represents the output list.
maplist(Predicate, [H1|T1], [H2|T2]) :-
    % Execute the predicate on the first input list item
    % to get the first output list item.
    call(Predicate, H1, H2),

    % Process the tail of the input list
    % to get the tail of the output list.
    maplist(Predicate, T1, T2).

*/

same_characters :-
  L1 = abc,
  L2 = [a, b, c],
  L1 =:= L2.

same_lists :-
  L1 = [red, green, blue],
  L2 = [red | [green | [blue | []]]],
  % L3 is .(red, .(green, .(blue, []))),
  L1 = L2.
  % L2 = L3.

sum([], 0).
sum([H|T], Sum) :-
  sum(T, Sum0), 
  Sum is H + Sum0.

% This is a reimplementation of the builtin "append" rule.
% Appending an empty list to any list gives the second list.
list_append([], L, L).

% Appending two lists is the same as appending
% the head of the first list (H) to the result of appending
% the tail of the first list (L1) to the second list (L2).
list_append([H|L1], L2, [H|L3]) :- list_append(L1, L2, L3).
