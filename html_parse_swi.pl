% This was only tested in SWI-Prolog.
:- use_module(library(http/http_open)). % for http_open

indent_write(Indent, V) :-
  format('~*|~t~w~n', [Indent, V]).

% This is used if 2nd argument is an element structure.
% element structure components are Tag, Attributes, and Children.
print_tag(Level, element(Tag, _, Children)) :-
  Indent is Level * 2,
  indent_write(Indent, Tag),
  NextLevel is Level + 1,
  maplist(print_tag(NextLevel), Children).

% This is used if 2nd argument is not an element structure.
print_tag(_, _). % ignore

% This provides a starting level of 0.
print_tag(E) :- print_tag(0, E).

process(In) :-
  % copy_stream_data(In, user_output). % for debugging
  load_html(In, DOM, []),
  maplist(print_tag, DOM).

shutdown(In) :- close(In), halt.

:- initialization(
  setup_call_cleanup(
    % Must use single, not double quotes around URL!
    http_open('https://mvolkmann.github.io', In, []),
    process(In),
    shutdown(In)
  )
).
