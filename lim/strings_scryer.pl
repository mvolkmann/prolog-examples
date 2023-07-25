% This requires the libraries list and reif
% which are included in ~/.scryerrc.

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

% prefix(split(Prefix, Suffix)) --> seq(Prefix), ".", seq(Suffix).
split(Delimiter, Prefix, Suffix) --> seq(Prefix), Delimiter, seq(Suffix).
filename_extension(Filename, Extension) --> split(".", Filename, Extension).
