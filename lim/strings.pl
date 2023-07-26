% This is intended to be run in Scryer Prolog which
% defines the seq predicate in its dcgs library.

% DCG rules are passed two more arguments than appear.
:- module(strings, [filename_extension/4, split/5]).

:- use_module(library(dcgs)).

% To test this, enter phrase(split(",", P, S), "foo,bar").
split(Delimiter, Prefix, Suffix) --> seq(Prefix), Delimiter, seq(Suffix).

% To test this, enter phrase(filename_extension(F, E), "foo.bar").
filename_extension(Filename, Extension) --> split(".", Filename, Extension).
