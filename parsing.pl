% This is in the "Lexical Analysis" section in my Prolog blog page.
% This was only tested in Scryer Prolog.
:- use_module(library(charsio)). % for char_type

assignment(V, I) --> ws, word(V), ws, ":=", ws, integer(I), ws.
% once(phrase(assignment(V, I), "  gretzky := 99 ")).
% V = "gretzky", I = 99.

% This matches any single digit.
% The char_type predicate is defined in the charsio library.
digit(D) --> [D], { char_type(D, decimal_digit) }.

% This matches any non-empty list of digits.
digits([D|Ds]) --> digit(D), digits_(Ds).

% This matches any list of digits including an empty list.
digits_([D|Ds]) --> digit(D), digits_(Ds).
digits_([]) --> [].

% This matches any non-empty list of digits and converts it to an integer.
% For example, once(phrase(integer(I), "123")) gives the integer 123.
% For example, once(phrase((ws, integer(I), ws), "  1961 ")).
% I = 1961.
integer(I) --> digits(Ds), { number_chars(I, Ds) }.

% See https://github.com/mthom/scryer-prolog/discussions/1921
% There may be a bug related to ascii_punctuation.
punctuation(P) --> [P], { char_type(P, ascii_punctuation) }.

% This is an "eager consumer rule" which
% causes tokens to extend to their maximum length.
% Eager consumer rules check for the nil case ([]) last.
word([H|T]) --> [H], { char_type(H, alphabetic) }, word(T).
word([]) --> [].

% For example, once(phrase(words(Ws), "This is a test")).
% gives ["This","is","a","test"].
% This will not terminate if it encounters
% an unexpected character such as punctuation.
% TODO: Try to fix this with the punctuation grammar rule above.
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

