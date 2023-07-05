% This was only tested in Scryer Prolog.
:- use_module(library(charsio)). % for char_type

/*
expr --> "1", expr_rest.
expr_rest --> [].
expr_rest --> "+", expr.
% phrase(expr, "1+1+1"), false. % adding , false allows it to terminate
*/

% This uses the "eager consumer rule" which causes
% tokens to extend to their maximum length.
% Eager consumes check for the nil case ([]) last.
% Curly braces on needed here to evaluate a Prolog predicate in a DCG rule.
% The char_type predicate here asserts that H is an alphabetic character.
word([H|T]) --> [H], { char_type(H, alphabetic) }, word(T).
word([]) --> [].

words([]) --> [].
words([H|T]) --> ws, word(H), ws, words(T).

ws --> [W], { char_type(W, whitespace) }, ws | [].

% The once predicate wraps another predicate and gives only the first solution.
% once(phrase(words(Ws), "The quick brown fox jumps over the lazy dog")).
% This does not terminate if the string contains
% characters that are not alphabetic or whitespace such as a period.

integer(I) --> digits(Ds), { number_chars(I, Ds) }.

digits([H|T]) --> digit(H), digits_remaining(T).
digits_remaining([H|T]) --> digit(H), digits_remaining(T).
digits_remaining([]) --> [].

digit(D) --> [D], { char_type(D, decimal_digit) }.

% once(phrase((ws, integer(I), ws), "  1961 ")).
% I = 1961.

assignment(V, I) --> ws, word(V), ws, ":=", ws, integer(I), ws.
% once(phrase(assignment(V, I), "  gretzky := 99 ")).
% V = "gretzky", I = 99.
