% See https://www.youtube.com/watch?v=CvLsVfq6cks at 26:57.
% This works if I copy each line to the Scryer top level,
% but it doesn't work if I load this file. Why?
:- use_module(library(http/http_open)). % for http_open
:- use_module(library(sgml)). % for load_html
:- use_module(library(xpath)). % for xpath

% This next line is not needed because it is in $HOME/.scryerrc.
% :- use_module(library(dcgs)). % for phrase

:- http_open("https://mvolkmann.github.io", S, []),
   load_html(stream(S), DOM, []),
   xpath(DOM, //li(text), Item),
   writeln(Item).

   /*
   phrase(tags(DOM), Ts),
   writeln(Ts).
   */
