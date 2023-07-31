:- use_module(library(dcgs)).
:- use_module(library(http/http_server)).
:- initialization(consult(family)).

body(Children) --> "<body>", children(Children), "</body>".
children([]) --> [].
children([H|T]) --> H, children(T).
div(Content) --> "<h1>", Content, "</h1>".
h1(Content) --> "<h1>", Content, "</h1>".
h2(Content) --> "<h2>", Content, "</h2>".
head(Content) --> "<head>", Content, "</head>".
html(Head, Body) --> "<html>", Head, Body, "</html>".
li(Content) --> "<li>", Content, "</li>".
title(Content) --> "<title>", Content, "</title>".
ul(Children) --> "<ul>", children(Children), "</ul>".

echo_handler(_, Response) :-
  http_status_code(Response, 200),
  http_body(Response, text("Welcome to Scryer Prolog!")).

favicon_handler(_, Response) :-
  http_status_code(Response, 200),
  http_body(Response, text("")). % not providing an icon

person_li(Person, Li) :-
  atom_chars(Person, Cs),
  Li = li(Cs).

grandchildren_handler(_, Response) :-
  % findall gathers all the solutions from the 2nd argument query,
  % transforms them with the first argument,
  % and places the resulting list in the 3rd argument.
  % findall(li(P)), grandfather(richard, P), L),
  findall(person_li(P), grandfather(richard, P), L),
  format("L = ~w~n", [L]),

  Title = "Grandchildren of Richard",
  phrase(html(
    head(title(Title)),
    body([
      h1(Title),
      ul([
        li("red"),
        li("green"),
        li("blue")
      ]),
      ul(L)
    ])
  ), Content),
  http_body(Response, text(Content)). % not providing an icon

listen :-
  % This cannot be stopped with ctrl-c.
  % See https://github.com/mthom/scryer-prolog/issues/485.
  % As a workaround, run the command `killall scryer-prolog`.
  http_listen(8081, [
    % GET /echo
    get(echo, echo_handler),
    get('favicon.ico', favicon_handler),
    get(grandchildren, grandchildren_handler)
  ]).
