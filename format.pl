:- use_module(library(dcgs)).
:- use_module(library(format)).

my_format(FS, Args, Result) :-
  phrase(format_(FS, Args), Result).

:- initialization((
  S = 'test string',
  my_format("S is ~w.~n", [S], Result),
  write(Result)
)).
