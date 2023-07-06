% This was only tested in Scryer Prolog.
% See https://www.youtube.com/watch?v=CvLsVfq6cks at 26:57.
:- use_module(library(charsio)). % for format_
:- use_module(library(format)). % for format_
:- use_module(library(http/http_open)). % for http_open
:- use_module(library(lists)). % for maplist
:- use_module(library(sgml)). % for load_html
:- use_module(library(xpath)). % for xpath

% The Scryer version of format wasn't working for me.
% This is a replacement that sort of works.
fmt(Fs, Args) :-
  phrase(format_(Fs, Args), Out),
  write(Out).

% N occurrences of the ASCII code A is described by the string S. 
% For example, repeat(5, 32, S). sets S to a string containing 5 spaces.
repeat(N, A, S) :-
  length(L, N),
  maplist(=(A), L),
  atom_codes(S, L).

% element structure components are Tag, Attributes, and Children.
print_tag(Level, element(Tag, _, Children)) :-  %
  Indent is Level * 2,
  repeat(Indent, 32, Spaces),
  write(Spaces), write(Tag), nl,
  NextLevel is Level + 1,
  maplist(print_tag(NextLevel), Children).

% This is used if 1st argument is not an element structure.
print_tag(_, _). % ignore

print_tag(E) :- print_tag(0, E).

/*
Example usage:
:- http_open("https://mvolkmann.github.io", S, []),
   load_html(stream(S), DOM, []),
   maplist(print_tag, DOM).
*/

/*
xpath(DOM, //li(text), Item),
writeln(Item).

phrase(tags(DOM), Ts),
writeln(Ts).
*/
