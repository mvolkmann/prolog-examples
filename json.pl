:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(si)).

% has_args(X, Args) :- X =.. [_|Args].

% Technically a double-quoted string is a list of character atoms,
% but we want to consider that to be a string.
% Otherwise we could use the `list_si` predicate.
list_not_chars(X) :-
  list_si(X),
  ( X = [] ->
    true
  ; \+ chars_si(X)
  ).

% This relates a structure to its functor.
% For example, `structure_functor(a(b, c), F)`
% will set F to "a/2".
structure_functor(Structure, Functor) :-
  functor(Structure, Name, Arity),
  write('Name ='), write(Name), nl,
  atom_chars(Name, NameC),
  write('NameC ='), write(NameC), nl,
  number_chars(Arity, ArityC),
  append(NameC, "/", Functor0),
  append(Functor0, ArityC, Functor),
  format("structure_functor: Functor = ~w~n", [Functor]).

arg_json(Arg, ArgJson) :-
  % phrase(json(Arg), ArgJson).
  ArgJson = "foo".

args_json(Args, ArgList) :-
  maplist(arg_json, Args, JsonArgs),
  append(JsonArgs, ArgList).

% To test this, enter something like the following and see the value of A.
% S = a(b,c), phrase(json(S), C), atom_chars(A, C).
json(Structure) -->
  "{",
  {
    Structure =.. [_|Args],
    length(Args, L),
    L > 0,
    structure_functor(Structure, Functor),
    args_json(Args, ArgsJson)
  },
  "\"_functor\": \"",
  seq(Functor),
  "\", \"_args\": [",
  seq(ArgsJson),
  "]",  
  "}".
% atom_chars(Functor, FunctorC),
  
json(Integer) -->
  [Integer],
  { integer_si(Integer) }.
  
json(List) -->
  List,
  { list_not_chars(List) }.
  
json(String) -->
  ["], String, ["],
  { chars_si(String) }.
  
demo :-
  phrase(json(19), J),
  write(J), nl.

report(Structure) :-
  functor(Structure, Name, Arity),
  format("Name = ~w~n", [Name]),
  format("Arity = ~w~n", [Arity]),
  arg(1, Structure, Arg),
  format("First Arg = ~w~n", [Arg]),
  Structure =.. List,
  format("List = ~w~n", [List]).
