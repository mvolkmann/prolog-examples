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

seq([]) --> [].
seq([H|T]) --> [H], seq(T).

% To use this, enter something like the following:
% phrase(hello(Name), "Hello, World!").
hello(Name) -->
  "Hello, ",
  seq(Cs),
  "!",
  { atomics_to_string(Cs, Name), ! }. % cut is needed to make it terminate

% To use this, enter something like the following:
% phrase(player(Name, Number), "Player Gretzky wears number 99.").
player(Name, Number) -->
  "Player ",
  seq(L1),
  " wears number ",
  seq(L2),
  ".",
  {
    atomics_to_string(L1, Name),
    atomics_to_string(L2, Number),
    ! % cut is needed to make it terminate
  }.

