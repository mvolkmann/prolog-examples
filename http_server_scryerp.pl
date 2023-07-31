:- use_module(library(dcgs)).
:- use_module(library(http/http_server)).
:- initialization(consult(family)).

tag1(Name, Content) --> "<", Name, ">", Content, "</", Name, ">".
tag2(Name, Children) --> "<", Name, ">", children(Children), "</", Name, ">".

children([]) --> [].
children([H|T]) --> H, children(T).

a(URL, Text) --> "<a href=\"", URL, "\">", Text, "</a>".
body(Content) --> tag2("body", Content).
div(Content) --> "<h1>", Content, "</h1>".
h1(Content) --> tag1("h1", Content).
h2(Content) --> tag1("h2", Content).
head(Content) --> tag2("head", Content).
html(Head, Body) --> "<html>", Head, Body, "</html>".
li(Content) --> tag1("li", Content).
style(Content) --> tag2("style", Content).
title(Content) --> tag1("title", Content).
ul(Content) --> tag2("ul", Content).

home_handler(_, Response) :-
  % http_status_code(Response, 200), % default status
  % http_body(Response, text("Welcome to Scryer Prolog!")).
  phrase(html(
    head([]),
    body([
      h1("Welcome to Scryer Prolog!"),
      a("/grandchildren", "Grandchildren")
    ])
  ), Content),
  http_body(Response, text(Content)).

not_found_handler(_, Response) :-
  http_status_code(Response, 404). % not providing an icon

person_li(Person, Li) :-
  atom_chars(Person, Cs),
  Li = li(Cs).

grandchildren_handler(_, Response) :-
  findall(P, grandfather(richard, P), Ps),
  maplist(person_li, Ps, Lis),

  Title = "Grandchildren of Richard",
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

listen :-
  % This cannot be stopped with ctrl-c.
  % See https://github.com/mthom/scryer-prolog/issues/485.
  % As a workaround, run the command `killall scryer-prolog`.
  http_listen(8081, [
    get('/', home_handler),
    get('favicon.ico', not_found_handler),
    get(grandchildren, grandchildren_handler)
  ]).
