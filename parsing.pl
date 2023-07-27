% TODO: INCLUDE THIS IN BLOG!!!
% This was only tested in Scryer Prolog.
:- use_module(library(charsio)). % for char_type

/*
expr --> "1", expr_rest.
expr_rest --> [].
expr_rest --> "+", expr.
% phrase(expr, "1+1+1"), false. % adding , false allows it to terminate
*/

% The once predicate wraps another predicate and gives only the first solution.
% For example,
% once(phrase(words(Ws), "The quick brown fox jumps over the lazy dog")).
% This does not terminate if the string contains
% characters that are not alphabetic or whitespace such as a period.

assignment(V, I) --> ws, word(V), ws, ":=", ws, integer(I), ws.
% once(phrase(assignment(V, I), "  gretzky := 99 ")).
% V = "gretzky", I = 99.
%
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

% This uses the "eager consumer rule" which causes
% tokens to extend to their maximum length.
% Eager consumers check for the nil case ([]) last.
word([H|T]) --> [H], { char_type(H, alphabetic) }, word(T).
word([]) --> [].

words([]) --> [].
words([H|T]) --> ws, word(H), ws, words(T).

% ws matches one or more whitespace characters which include
% space, tab (\t), newline (\n), formfeed (\f), carriage return (\r),
% and many higher Unicode characters that are considered whitespace.
% To see a full list, enter
% length(_, N), char_code(C, N), char_type(C, whitespace).
% length(_, N) is a tricky way to generate decimal numbers from 0 to infinity.
% char_code(C, N) then gets the character code
% that corresponds to each decimal number.
% char_type(C, whitespace) then filters the output
% to only the character codes that are whitespace characters.
ws --> [W], { char_type(W, whitespace) }, ws.
ws --> [].

% once(phrase((ws, integer(I), ws), "  1961 ")).
% I = 1961.
