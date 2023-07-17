/*
This is a modification of the code in nlp.pl
that captures parsed results in a syntax tree.
It is based on code in the video "Build Syntax Trees in Prolog with DCGs"
at https://youtu.be/QGXypIkV-GU.

To capture the atoms that matches each grammar rule,
we specify a structure as the argument of each rule.
These structures can contain arguments that are fixed atoms
or variables that are set in the grammar rule body.

This enables each grammar rule to generate a tree of structures
that describe what was matched.
*/

:- include(sentences).

determiner(d(a)) --> [a].
determiner(d(the)) --> [the].

noun(n(cat)) --> [cat].
noun(n(dog)) --> [dog].
noun_phrase(np(D, N)) --> determiner(D), noun(N).
verb(v(chased)) --> [chased].
verb_phrase(vp(V, Np)) --> verb(V), noun_phrase(Np).
sentence(s(Np, Vp)) --> noun_phrase(Np), verb_phrase(Vp).

% To see a sample tree, enter
% `phrase(sentence(Tree), [a,cat,chased,the,dog]).`

% Enter `test.`
test :-
  phrase(sentence(Tree), [the,cat,chased,a,dog]), % matches
  \+ phrase(sentence(Tree), [the,cat,chased,a,mouse]), % does not match
  !.
  
% Enter `complete1.`
complete1 :-
  findall(X, phrase(sentence(Tree), [the,X,chased,the,dog]), Solutions),
  format('Solutions = ~w~n', [Solutions]).

% Enter `complete2.`
complete2 :-
  findall(Rest, phrase(sentence(Tree), [the,cat,chased | Rest]), Solutions),
  format('Solutions = ~w~n', [Solutions]).

% Enter `generate.`
generate :-
  findall(X, phrase(sentence(Tree), X), Solutions),
  maplist(atoms_sentence, Solutions, Sentences),
  maplist(writeln, Sentences).

