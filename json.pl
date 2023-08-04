:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(si)).
:- initialization(consult(strings_scryer)).

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
  atom_chars(Name, NameC),
  number_chars(Arity, ArityC),
  append(NameC, "/", Functor0),
  append(Functor0, ArityC, Functor).

arg_json(Arg, Json) :-
  format("arg_json: Arg = ~w~n", [Arg]),
  phrase(json(Arg), Json),
  format("arg_json: Json = ~w~n", [Json]).

args_json(Args, Json) :-
  format("args_json: Args = ~w~n", [Args]),
  maplist(arg_json, Args, JsonArgs),
  format("args_json: JsonArgs = ~w~n", [JsonArgs]),
  string_list(Json, ',', JsonArgs),
  format("args_json: Json = ~w~n", [Json]).

json(Atom) -->
  ["], seq(Chars), ["],
  {
    atom_si(Atom),
    format("json atom: Atom = ~w~n", [Atom]),
    atom_chars(Atom, Chars),
    format("json atom: Chars = ~w~n", [Chars])
  }.
  
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
  "]}".  
  
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
