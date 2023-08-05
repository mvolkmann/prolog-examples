:- use_module(library(dcgs)).

% phrase(sample(S), X).
sample(S) -->
  seq(First),
  {
    First = "one",
    Second = "two",
    S = "three"
  },
  seq(Second).
