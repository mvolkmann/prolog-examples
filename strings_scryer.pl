:- use_module(library(charsio)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(reif)).

chars_capitalized([], []).
chars_capitalized([H1|T], Atom):-
  % The to_upper argument gets set to a list,
  % but we only want the first character.
  char_type(H1, to_upper([H2|_])),
  atom_chars(Atom, [H2|T]).

split("", _, []).
% Delimiter must be a single character atom.
split(String, Delimiter, Parts) :-
  if_(
    memberd_t(Delimiter, String),
    % then part
    (
      [First|Rest1] = Parts,
      once(append(First, [Delimiter|Rest2], String)),
      split(Rest2, Delimiter, Rest1)
    ),
    % else part
    Parts = [String]
  ).

% This approach uses DCGs.
% prefix(split(Prefix, Suffix)) --> seq(Prefix), ".", seq(Suffix).
% split(Delimiter, Prefix, Suffix) --> seq(Prefix), Delimiter, seq(Suffix).
% filename_extension(Filename, Extension) --> split(".", Filename, Extension).

% This approach does not use DCGs.
split(S, Delimiter, Prefix, Suffix) :-
  once(append(Prefix, [Delimiter|Suffix], S)).

% Example: filename_extension("foo.bar", F, E).
% gives F = "foo", E = "bar"
filename_extension(S, Filename, Extension) :-
  split(S, ., Filename, Extension).
