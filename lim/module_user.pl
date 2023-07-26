% The strings module define in strings.pl is specific to Scryer Prolog.
:- use_module(strings).
:- use_module(library(format)).

:- initialization((
  phrase(filename_extension(F, E), "foo.bar"),
  format("F = ~s, E = ~s~n", [F, E])
)).
