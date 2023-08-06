:- module(list_util, [every/2, list_without/3, some/2]).

:- use_module(library(lists)).
:- use_module(library(reif)). % for if_ and tfilter

:- meta_predicate every(1, ?).
every(Predicate, List) :- maplist(Predicate, List).

:- meta_predicate some(1, ?).
% This succeeds if at least one element in List satisfies Predicate
% and fails otherwise.
some(_, []) :- fail.
some(Predicate, [H|T]) :-
  (call(Predicate, H) ->
    true
  ; some(Predicate, T)
  ).

/*
% Removing anything from an empty list matches an empty list.
list_without([], _, []).

% H is the head of the 1st argument list and T is its tail.
% E is the element to removed.
% L is a list containing all elements of the first list
% with all occurrences of E removed.
list_without([H|T], E, L) :-
  % If H matches E ...
  ( H == E ->
    % Then the result list L is the same as T with E removed.
    list_without(T, E, L)
    % Otherwise the result list L is a new list with head H
    % and tail is the same as T with E removed.
  ; list_without(T, E, L2),
    L = [H|L2]
  ).
*/

  /* Supposedly if_ can be used instead and has advantages.
   But I can't figure out how to use it.
  if_(
    H == E,
    list_without(T, E, L),
    (list_without(T, E, L2), L = [H|L2])
  ).
  */

list_without(Ls0, E, Ls) :- tfilter(dif(E), Ls0, Ls).
