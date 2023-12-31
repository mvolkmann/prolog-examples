:- use_module(library(dcg/basics)). % for string_without and digits

/*
% If we match "cat", there is no need to check for also matching "dog".
pet --> "cat", { ! } | "dog".

% This matches a sequence of pets separated by single spaces.
% After exhausting all matches, we can stop checking for more.
% To use this, enter something like the following:
% phrase(raining, `cat cat dog dog cat`).
raining --> pet, " ", raining, { ! }.
raining --> pet.
*/

% For simple text matching and extraction,
% a regular expression is an easier alternative.
% To use this, enter something like the following:
% phrase(hello(Name), "Hello, World!").
hello(Name) -->
  "Hello, ", string(S), "!", !,
  { string_codes(Name, S) }.

% To use this, enter something like the following:
% phrase(player(Name, Number), "Player Gretzky wears number 99.").
player(Name, Number) -->
  "Player ",
  string_without(" ", Cs),
  " wears number ",
  digits(Ds),
  ".",
  {
    string_codes(Name, Cs),
    number_codes(Number, Ds)
  }.
