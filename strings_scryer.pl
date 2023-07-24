:- use_module(library(lists)).
:- use_module(library(reif)).

split("", _, []).
split(String, Char, Segments0) :-
  if_(
    memberd_t(Char, String),
    (
      Segments0 = [Segment|Segments],
      once(append(Segment, [Char|Rs], String)),
      split(Rs, Char, Segments)
    ),
    Segments0 = [String]
  ).

