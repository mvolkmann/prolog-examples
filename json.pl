:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(si)).
:- initialization(consult(strings_scryer)).

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

json(Atom) -->
  "\"",
  {
    atom_si(Atom),
    format("json Atom: Atom = ~w~n", [Atom]),
    atom_chars(Atom, Chars),
    format("json Atom: Chars = ~w~n", [Chars])
  },
  seq(Chars),
  "\"".
  
json(Integer) -->
  [Integer],
  {
    integer_si(Integer),
    format("json: Integer = ~w~n", [Integer])
  }.
  
json(List) -->
  "[",
  {
    list_not_chars(List),
    format("json List: List = ~w~n", [List]),
    values_json(List, Json),
    format("json List: Json = ~w~n", [Json])
  },
  seq(Json),
  "]".
  
% To test this, enter something like the following and see the value of A.
% S = a(b,c), phrase(json(S), C), atom_chars(A, C).
json(Structure) -->
  "{",
  {
    format("json: Structure = ~w~n", [Structure]),
    Structure =.. [_|Args],
    length(Args, L),
    L > 0,
    structure_functor(Structure, Functor),
    values_json(Args, ArgsJson)
  },
  "\"_functor\": \"",
  seq(Functor),
  "\", \"_args\": [",
  seq(ArgsJson),
  "]}".  
  
% This must appear after json(Structure), but I don't know why.
json(String) -->
  "\"", String, "\"",
  {
    chars_si(String),
    format("json: String = ~w~n", [String])
  }.
  
value_json(Value, Json) :-
  format("value_json: Value = ~w~n", [Value]),
  once(phrase(json(Value), Json)),
  format("value_json: Json = ~w~n", [Json]).

values_json(Values, Json) :-
  format("values_json: Values = ~w~n", [Values]),
  % Convert Values list to JSON list.
  maplist(value_json, Values, Jsons),
  format("values_json: Jsons = ~w~n", [Jsons]),
  % Create string that is a comma-separated list of the JSON values.
  string_list(Json, ',', Jsons),
  format("values_json: Json = ~w~n", [Json]).

% For debugging
report(Structure) :-
  functor(Structure, Name, Arity),
  format("Name = ~w~n", [Name]),
  format("Arity = ~w~n", [Arity]),
  arg(1, Structure, Arg),
  format("First Arg = ~w~n", [Arg]),
  Structure =.. List,
  format("List = ~w~n", [List]).