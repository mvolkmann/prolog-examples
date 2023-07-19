% This uses DCGs.
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

% If we match "cat", there is no need to check for also matching "dog".
pet --> "cat", { ! } | "dog".

% This matches a sequence of pets separated by single spaces.
% After exhausting all matches, we can stop checking for more.
raining --> pet, " ", raining, { ! }.
raining --> pet.

% This gather a sequence of characters into a list of character atoms.
seq([]) --> [].
seq([H|T]) --> [H], seq(T).

% This gather a sequence of characters into a number.
number_seq(N) --> seq(Cs), { atomics_to_string(Cs, S), number_string(N, S) }.

% This gather a sequence of characters into a string.
string_seq(S) --> seq(Cs), { atomics_to_string(Cs, S) }.

% To use this, enter something like the following:
% phrase(hello(Name), "Hello, World!").
hello(Name) -->
  "Hello, ",
  string_seq(Name),
  % string_without(" ", Cs), % This does not work!
  "!",
  { ! }. % cut is needed to make it terminate

% To use this, enter something like the following:
% phrase(player(Name, Number), "Player Gretzky wears number 99.").
player(Name, Number) -->
  "Player ",
  string_seq(Name),
  " wears number ",
  number_seq(Number),
  ".",
  { ! }. % cut is needed to make it terminate

/*
ws --> [W], { char_type(W, whitespace) }, ws.
ws --> [].

:- initialization
  writeln('running'),
  length(L, _),
  phrase(raining, L).
  % maplist(atomics_to_string, L, S).
*/
