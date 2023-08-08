sign_word1(N, Word) :-
  ( N =:= 0 ->
    Word = zero
  ; N > 0 ->
    Word = positive
  ; Word = negative
  ).

sign_word2(N, Word) :-
  ( N =:= 0
  , Word = zero
  ; N > 0
  , Word = positive
  ; Word = negative
  ).

is_zero(N, true) :- N =:= 0.
is_zero(N, false) :- N =\= 0.

is_positive(N, true) :- N > 0.
is_positive(N, false) :- N =< 0.

sign_word3(N, Word) :-
  if_(
    is_zero(N),
    Word = zero,
    if_(
      is_positive(N),
      Word = positive,
      Word = negative
    )
  ).

writeln(X) :- write(X), nl.

demo :-
  sign_word3(5, W1),
  writeln(W1),
  sign_word3(-5, W2),
  writeln(W2),
  sign_word3(0, W3),
  writeln(W3).
