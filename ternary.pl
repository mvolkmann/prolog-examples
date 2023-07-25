sign_word(N, Word) :-
  (N =:= 0 -> Word = 'zero';
  (N > 0 -> Word = 'positive';
  Word = 'negative')).

sign_wordX(N, Word) :-
  (N =:= 0, Word = 'zero';
  (N > 0, Word = 'positive';
  Word = 'negative')).

:- initialization((
  sign_word(5, W1),
  writeln(W1),
  sign_word(-5, W2),
  writeln(W2),
  sign_word(0, W3),
  writeln(W3),

  writeln('before'),
  (W3 == 'zero' -> writeln('got zero'); true),
  writeln('after'),
  halt
)).
