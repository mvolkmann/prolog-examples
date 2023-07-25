:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).

:- initialization((
  S = 'test string',

  % Using format_ instead of format captures the result
  % instead of writing it to stdout.
  phrase(format_("S is ~w.~n", [S]), Result),

  % Result is a list of character atoms.
  % This writes each one to stdout.
  maplist(write, Result)
)).
