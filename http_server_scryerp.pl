:- use_module(library(http/http_server)).

text_handler(_, Response) :-
  http_status_code(Response, 200),
  http_body(Response, text("Welcome to Scryer Prolog!")).

:- initialization(
  % GET /echo
  http_listen(8081, [
    get(echo, text_handler)
  ])
).
