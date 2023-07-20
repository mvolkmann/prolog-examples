% This uses DCGs.
% NOTE: It seems important for double_quotes to be set to
%       codes for the grammar rules in dcg/basics to work!
:- set_prolog_flag(double_quotes, codes).
:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

% From dcg/basics:
% digit(C) --> [C], { code_type(C, digit) }.
letter(C) --> [C], { code_type(C, alpha) }.
lower(C) --> [C], { code_type(C, lower) }.
upper(C) --> [C], { code_type(C, upper) }.
letter_or_digit(C) --> letter(C) | digit(C).

identifier_([]) --> [].
identifier_(I) --> letter_or_digit(C), identifier_(T), { I = [C|T] }.
identifier(I) --> letter(C), identifier_(T), { atom_codes(I, [C|T]) }.

% white is a space or tab.
% eol is \n, \r\n, or end of input.
ws1 --> white | eol.
% ws matches zero or more ws1 characters.
ws --> [].
ws --> ws1, ws.

arguments(ArgList) --> ws, identifier(A), ws, { ArgList = [A] }.
arguments(ArgList) --> ws, identifier(A), ws, ", ", ws, arguments(As), { ArgList = [A|As] }.

value(V) --> identifier(V) | integer(V).

operator(+) --> "+".
operator(-) --> "-".
operator(*) --> "*".
operator(/) --> "/".

math(math(O, V1, V2)) --> value(V1), ws, operator(O), ws, value(V2).

assignment(assign(I, V)) --> identifier(I), ws, "=", ws, value(V).
assignment(assign(I, M)) --> identifier(I), ws, "=", ws, math(M).

print(print(V)) --> "print", ws, value(V).

statement_(S) --> (assignment(S) | print(S)) .
statement(S) --> whites, statement_(S), whites.
statement(S) --> whites, statement_(S), ws.
statements(Body) --> statement(S), { Body = [S] }.
statements(Body) --> statement(S), eol, statements(Ss), { Body = [S|Ss] }.

% To use this, enter something like the following:
% once(phrase(function(F, A, S), "fn foo(a, b)\nc = a * b\nprint c\nend")).
function(Name, ArgList, Statements) -->
  "fn ", identifier(Name), "(", arguments(ArgList), ")", ws,
  statements(Statements),
  ws, "end", ws.

/*
:- initialization
  phrase_from_file(function(N, A, S), 'dcg4.txt').
*/
