:- module(strings, [
    chars_capitalized/2,
    filename_extension/3,
    split/4,
    string_list/3
  ]).

:- use_module(library(charsio)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(reif)). % for if_ and memberd_t

chars_capitalized([], []).
chars_capitalized([H1|T], Atom):-
  % The to_upper argument gets set to a list,
  % but we only want the first character.
  char_type(H1, to_upper([H2|_])),
  atom_chars(Atom, [H2|T]).

% Example: filename_extension("foo.bar", F, E).
% gives F = "foo", E = "bar"
filename_extension(S, Filename, Extension) :-
  split(S, ., Filename, Extension).

% This approach does not use DCGs.
split(S, Delimiter, Prefix, Suffix) :-
  once(append(Prefix, [Delimiter|Suffix], S)).

% This relates a string to list of string parts
% obtained by splitting on a given delimiter
% which is a single character atom.
% For example:
% string_list("foo,bar,baz", ',', L).
% gives L = ["foo","bar","baz"].
% and
% string_list(S, ',', ["foo","bar","baz"]).
% gives S = "foo,bar,baz".
string_list(String, Delimiter, Parts) :-
  once(string_list_(String, Delimiter, Parts)).
string_list_("", _, []).
string_list_(String, Delimiter, Parts) :-
  if_(
    memberd_t(Delimiter, String),
    % then part
    (
      Parts = [Before|Parts0], % must be first to terminate
      % Get parts of String before and after Delimiter.
      once(append(Before, [Delimiter|After], String)),
      % Recursively process After.
      string_list_(After, Delimiter, Parts0)
    ),
    % else part
    Parts = [String]
  ).

% This approach uses DCGs.
% prefix(split(Prefix, Suffix)) --> seq(Prefix), ".", seq(Suffix).
% split(Delimiter, Prefix, Suffix) --> seq(Prefix), Delimiter, seq(Suffix).
% filename_extension(Filename, Extension) --> split(".", Filename, Extension).