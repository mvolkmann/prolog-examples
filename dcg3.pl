% This uses DCGs.
% NOTE: It seems important for double_quotes to be set to
%       codes for the grammar rules in dcg/basics to work!
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

% If we match "cat", there is no need to check for also matching "dog".
pet --> "cat", { ! } | "dog".

% This matches a sequence of pets separated by single spaces.
% After exhausting all matches, we can stop checking for more.
% To use this, enter something like the following:
% phrase(raining, "cat cat dog dog cat").
raining --> pet, " ", raining, { ! }.
raining --> pet.

% This gather a sequence of characters into a list of character atoms.
seq([]) --> [].
seq([H|T]) --> [H], seq(T).

% This gather a sequence of characters into a number.
% When double_quotes is set the chars, use atomics_to_string.
% When double_quotes is set the codes, use number_codes.
% number_seq(N) --> seq(Cs), { atomics_to_string(Cs, S), number_string(N, S) }.
number_seq(N) -->
  seq(Cs),
  { length(Cs, L), (L == 0 -> N = 0; number_codes(N, Cs)) }.

% This gather a sequence of characters into a string.
% When double_quotes is set the chars, use atomics_to_string.
% When double_quotes is set the codes, use atom_codes.
% string_seq(S) --> seq(Cs), { atomics_to_string(Cs, S) }.
string_seq(S) --> seq(Cs), { atom_codes(S, Cs) }.

% For simple text matching and extraction,
% a regular expression is an easier alternative.
% To use this, enter something like the following:
% once(phrase(hello(Name), "Hello, World!")).
hello(Name) -->
  "Hello, ",
  string_seq(Name),
  "!".

% To use this, enter something like the following:
% once(phrase(player(Name, Number), "Player Gretzky wears number 99.")).
player(Name, Number) -->
  "Player ",
  string_seq(Name),
  " wears number ",
  number_seq(Number),
  %string_seq(Number),
  ".".

% From dcg/basics:
% digit(C) --> [C], { code_type(C, digit) }.
letter(C) --> [C], { code_type(C, alpha) }.
lower(C) --> [C], { code_type(C, lower) }.
upper(C) --> [C], { code_type(C, upper) }.
letter_or_digit(C) --> letter(C) | digit(C).

identifier_([]) --> [].
identifier_(I) --> letter_or_digit(C), identifier_(T), { I = [C|T] }.
% identifier(I) --> letter(C), identifier_(T), { I = [C|T] }.
identifier(I) --> letter(C), identifier_(T), { atom_codes(I, [C|T]) }.

% white is a space or tab.
% eol is \n, \r\n, or end of input.
ws1 --> white | eol.
% ws matches zero or more ws1 characters.
ws --> [].
ws --> ws1, ws.

arguments(ArgList) --> ws, identifier(A), ws, { ArgList = [A] }.
arguments(ArgList) --> ws, identifier(A), ws, ", ", ws, arguments(As), { ArgList = [A|As] }.

statement(stmt(demo)) --> "stmt".
statements(Body) --> statement(S), { Body = [S] }.
statements(Body) --> statement(S), ws, eol, statements(Ss), { Body = [S|Ss] }.

% To use this, enter something like the following:
% phrase(function(Name, ArgList), "fn foo(bar, baz)").
function(Name, ArgList) -->
  "fn ", identifier(Name), "(", arguments(ArgList), ") ",
  % statements,
  "end".

