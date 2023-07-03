% The substract predicate can be used to create a list
% that is derived by removing a given element from another list.
% This shows how we could implement that.

% Removing anything from an empty list matches an empty list.
list_without([], _, []).

% H is the head of the 1st argument list and T is its tail.
% E is the element to removed.
% L is a list containing all elements of the first list
% with all occurrences of E removed.
list_without([H|T], E, L) :-
  % If H matches E ...
  H == E ->
    % Then the result list L is the same as T with E removed.
    list_without(T, E, L);
    % Otherwise the result list L is a new list with head H
    % and tail is the same as T with E removed.
    list_without(T, E, L2), L = [H|L2].
