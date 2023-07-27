:- use_module(library(format)).

run :-
  File = "term.txt",
  Term = myStructure(a1, a2),
  Options = [],

  open(File, write, Stream1),
  write_term(Stream1, Term, Options),
  write(Stream1, '.'),
  close(Stream1),

  open(File, read, Stream2),
  read_term(Stream2, Term2, Options),
  close(Stream2),

  format("Term2 = ~w~n", [Term2]).
