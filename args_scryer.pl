:- use_module(library(format)).

:- initialization((
  argv(Args),
  format("Args = ~w~n", [Args]),
  halt
)).
