double(X, R) :- R is X * 2.
isEven(N) :- mod(N, 2) =:= 0.
isEven(N, R) :- R is mod(N, 2) = 0.
isOdd(N) :- mod(N, 2) =:= 1.
isOdd(N, R) :- R is mod(N, 2) = 1.

% When the input list is empty, the output list should be empty.
maplist(_, [], []).

% [H1|T1] represents the input list.
% [H2|T2] represents the output list.
maplist(Predicate, [H1|T1], [H2|T2]) :-
    % Execute the predicate on the first input list item
    % to get the first output list item.
    call(Predicate, H1, H2),

    % Process the tail of the input list
    % to get the tail of the output list.
    maplist(Predicate, T1, T2).

% Enter a query like `maplist(double, [1, 2, 3], X).`
% The result should be `X = [2, 4, 6]`.

prepend(Value, List, Result) :-
  Result is [Value | List].

sum(List, Sum) :-
  % If the list is empty then the sum is zero.
  List = [] -> Sum = 0;
  % Otherwise ...
  List =
    % Get the first number and a list of the remaining numbers.
    [Head|Tail],
    % Compute the sum of the remaining numbers.
    sum(Tail, TailSum),
    % The result is the first number plus that sum.
    Sum is TailSum + Head.
