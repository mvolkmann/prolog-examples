:- use_module(library(format)).

:- initialization((
  '$toplevel':argv(Args),
  format("Args = ~w~n", [Args]),
  halt
)).
