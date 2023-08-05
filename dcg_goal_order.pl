:- use_module(library(dcgs)).

% phrase(sample(S), X).
sample(S) -->
  % This grammar rule should appear BEFORE any
  % non-terminals that use the variables it sets.
  {
    First = "one",
    Second = "two",
    S = "three"
  },
  seq(First),
  seq(Second).
