% This uses DCGs.
:- set_prolog_flag(double_quotes, chars).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

/*
raining --> [].
raining --> ("cat" | "dog"), raining.

:- initialization
  writeln('running'),
  length(L, _),
  phrase(raining, L).
  % maplist(atomics_to_string, L, S).
*/

/*
ws --> [W], { char_type(W, whitespace) }, ws.
ws --> [].
*/

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

