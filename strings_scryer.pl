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

% This can be used to create a string by joining a list of
% other strings with a delimiter between each.
% For example: list_joined(["foo","bar","baz"], ',', S).
% This gives S = "foo,bar,baz"
list_joined([], _, "").
list_joined([H], _, H).
list_joined([H|T], Delimiter, Joined) :-
  list_joined(T, Delimiter, Joined0),
  append([H, [Delimiter], Joined0], Joined).

% This can be used to create a list by splitting a string on a delimiter.
% For example: string_list("foo,bar,baz", ',', L).
% This gives L = ["foo","bar","baz"].
string_list("", _, []).
% Delimiter must be a single character atom.
string_list(String, Delimiter, Parts) :-
  if_(
    memberd_t(Delimiter, String),
    % then part
    (
      [First|Rest1] = Parts,
      once(append(First, [Delimiter|Rest2], String)),
      string_list(Rest2, Delimiter, Rest1)
    ),
    % else part
    Parts = [String]
  ).

% This approach uses DCGs.
% prefix(split(Prefix, Suffix)) --> seq(Prefix), ".", seq(Suffix).
% split(Delimiter, Prefix, Suffix) --> seq(Prefix), Delimiter, seq(Suffix).
% filename_extension(Filename, Extension) --> split(".", Filename, Extension).

% This approach does not use DCGs.
string_list(S, Delimiter, Prefix, Suffix) :-
  once(append(Prefix, [Delimiter|Suffix], S)).

% Example: filename_extension("foo.bar", F, E).
% gives F = "foo", E = "bar"
filename_extension(S, Filename, Extension) :-
  split(S, ., Filename, Extension).
