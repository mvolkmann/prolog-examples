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
