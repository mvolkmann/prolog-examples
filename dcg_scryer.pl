:- use_module(library(dcgs)). % for -->, seq, and phrase
:- use_module(library(charsio)). % for char_type

% To use this, enter something like the following:
% phrase(hello(Name), "Hello, World!").
hello(Name) -->
  "Hello, ", seq(Name), "!", !.

% To use this, enter something like the following:
% phrase(player(Name, Number), "Player Gretzky wears number 99.").
player(Name, Number) -->
  "Player ",
  seq(Name),
  " wears number ",
  % seq(Number),
  integer(Number),
  ".".

% This matches any single digit.
% The char_type predicate is defined in the charsio library.
digit(D) --> [D], { char_type(D, decimal_digit) }.

% This matches any non-empty list of digits.
digits([D|Ds]) --> digit(D), digits_(Ds).

% This matches any list of digits including an empty list.
digits_([D|Ds]) --> digit(D), digits_(Ds).
digits_([]) --> [].

% This matches any non-empty list of digits and converts it to an integer.
integer(I) --> digits(Ds), { number_chars(I, Ds) }.

