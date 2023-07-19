% This uses DCGs.
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

% If we match "cat", there is no need to check for also matching "dog".
pet --> "cat", { ! } | "dog".
% After exhausting all matches, we can stop checking for more.
raining --> pet, " ", raining, { ! }.
raining --> pet.

% This gathers a sequence of arbitrary characters into a string.
seq(W) --> seq_(Cs), { atomics_to_string(Cs, W) }.
seq_([]) --> [].
seq_([H|T]) --> [H], seq_(T).

% To use this, enter something like the following:
% phrase(hello(Name), "Hello, World!").
hello(Name) -->
  "Hello, ",
  seq(Name),
  % string_without(" ", Cs), % This does not work!
  "!",
  { ! }. % cut is needed to make it terminate

% To use this, enter something like the following:
% phrase(player(Name, Number), "Player Gretzky wears number 99.").
player(Name, Number) -->
  "Player ",
  seq(Name),
  " wears number ",
  seq(Number),
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
