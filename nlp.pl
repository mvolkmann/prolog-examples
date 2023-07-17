% This is a basic example of performing Natural Language Processing (NLP) in Prolog.

:- set_prolog_flag(double_quotes, chars).

sentence --> noun_phrase, verb_phrase.
noun_phrase --> determiner, noun.
determiner --> [the] | [a].
noun --> [cat] | [dog].
verb --> [chased].
verb_phrase --> verb, noun_phrase.

write_sentence(sentence) :-
 writeln(sentence).

:- initialization
  findall(X, phrase(sentence, X), Bag),
  format('Bag = ~w~n', [Bag]),
  maplist(write_sentence, Bag).
  % maplist(writeln, Bag).

