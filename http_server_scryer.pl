:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(http/http_server)).
:- use_module(library(serialization/json)).

:- use_module(html_gen).
:- use_module('lib/json.pl').
:- use_module('lib/strings.pl').

:- initialization(consult(family)).

grandchildren_handler(Request, Response) :-
  % Get and print all request headers.
  http_headers(Request, Headers),
  maplist(print_pair, Headers),

  % Get and print the request body.
  http_body(Request, text(Body)),
  format("Body = ~w~n", [Body]),

  % Get the "name" query parameter.
  ( http_query(Request, "name", NameChars) ->
    have_name(Response, NameChars)
  ; missing_query_parameter(Response, "name")
  ).

grandchildren_json_handler(Request, Response) :-
  ( http_query(Request, "name", NameChars) ->
    % Get the grandchildren for the given name.
    atom_chars(NameAtom, NameChars),
    findall(P, grandfather(NameAtom, P), Ps),
    phrase(json(Ps), Json),
    http_headers(Response, ["Content-Type"-"application/json"]),
    http_body(Response, text(Json))
  ; missing_query_parameter(Response, "name")
  ).

have_grandchildren(Response, NameChars, Grandchildren) :-
  chars_capitalized(NameChars, Name),
  phrase(format_("Grandchildren of ~s!", [Name]), Title),
  maplist(person_li, Grandchildren, Lis),

  phrase(html(
    head([
      title(Title),
      style([
        "body { background-color: linen; }",
        "h1 { color: red; }",
        "h2 { color: blue; }",
        "li { color: purple; }"
      ])
    ]),
    body([
      h1(Title),
      ul(Lis),

      h2("Colors"),
      ul([
        li("red"),
        li("green"),
        li("blue")
      ])
    ])
  ), Content),
  http_body(Response, text(Content)). % not providing an icon

have_name(Response, NameChars) :-
  atom_chars(NameAtom, NameChars),
  % Get the grandchildren for the given name.
  findall(P, grandfather(NameAtom, P), Ps),
  length(Ps, Length),
  ( Length > 0 ->
    have_grandchildren(Response, NameChars, Ps)
  ; have_no_grandchildren(Response, NameChars)
  ).

have_no_grandchildren(Response, NameChars) :-
  chars_capitalized(NameChars, Name),
  phrase(format_("~w has no grandchildren.", [Name]), Content),
  http_body(Response, text(Content)). % not providing an icon

have_query(Response, QueryChars) :-
  format("have_query: Query = ~w~n", [QueryChars]),
  % atom_chars(QueryAtom, QueryChars),
  % findall(P, QueryAtom(P), Result),
  Result = foo(bar, baz),
  phrase(json(Result), Json),
  http_headers(Response, ["Content-Type"-"application/json"]),
  http_body(Response, text(Json)).

home_handler(_, Response) :-
  % http_status_code(Response, 200), % default status
  phrase(html(
    head([]),
    body([
      h1("Welcome to Scryer Prolog!"),
      a("/grandchildren", "Grandchildren") % hyperlink
    ])
  ), Content),
  http_body(Response, text(Content)).

json_handler(_, Response) :-
  Value = foo(alpha, bar(beta, baz(gamma))),
  format("Value = ~w~n", [Value]),
  phrase(json:json(Value), Chars),
  format("Chars = ~s~n", [Chars]),
  http_body(Response, text(Chars)).

listen :-
  % This cannot be stopped with ctrl-c.
  % See https://github.com/mthom/scryer-prolog/issues/485.
  % As a workaround, run the command `killall scryer-prolog`.
  http_listen(8081, [
    get('/', home_handler),
    get('favicon.ico', not_found_handler),
    get(grandchildren, grandchildren_handler),
    get('grandchildren.json', grandchildren_json_handler),
    get(json, json_handler),
    get('query', query_handler)
  ]).

missing_query_parameter(Response, Name) :-
  phrase(format_("query parameter \"~s\" is missing", [Name]), Content),
  http_status_code(Response, 400),
  http_body(Response, text(Content)). % not providing an icon

not_found_handler(_, Response) :-
  http_status_code(Response, 404). % not providing an icon

person_li(Person, Li) :-
  atom_chars(Person, Cs),
  Li = li(Cs).

% For debugging
print_pair(Name-Value) :-
  format("~s = ~s~n", [Name, Value]).

query_handler(Request, Response) :-
  ( http_query(Request, "q", Query) ->
    have_query(Response, Query)
  ; missing_query_parameter(Response, "q")
  ).

