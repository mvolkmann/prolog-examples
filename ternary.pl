sign_word(X, Y) :-
  (X = 0 -> Y = 'zero';
  (X > 0 -> Y = 'positive';
  Y = 'negative')).
