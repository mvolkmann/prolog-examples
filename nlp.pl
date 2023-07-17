% This is a basic example of performing Natural Language Processing (NLP) in Prolog.
:- include(sentences).

% From Wikipedia, "English determiners are words such as
% the, a, each, some, which, this, and six
% that are most commonly used with nouns to specify their referents."
determiner --> [the] | [a].

noun --> [cat] | [dog].
noun_phrase --> determiner, noun.
verb --> [chased].
verb_phrase --> verb, noun_phrase.
sentence --> noun_phrase, verb_phrase.

% Enter `test.`
test :-
  phrase(sentence, [the,cat,chased,a,dog]),
  \+ phrase(sentence, [the,cat,chased,a,mouse]),
  !.
  
% Enter `complete1.`
complete1 :-
  findall(X, phrase(sentence, [the,X,chased,the,dog]), Solutions),
  format('Solutions = ~w~n', [Solutions]).

% Enter `complete2.`
complete2 :-
  findall(Rest, phrase(sentence, [the,cat,chased | Rest]), Solutions),
  format('Solutions = ~w~n', [Solutions]).

% Enter `generate.`
generate :-
  findall(X, phrase(sentence, X), Solutions),
  maplist(atoms_sentence, Solutions, Sentences),
  maplist(writeln, Sentences).

