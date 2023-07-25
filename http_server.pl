:- use_module(library(http/http_server)).

:- initialization((
  consult(exercise1_3),
  http_server([port(8081)])
)).

:- http_handler(
  root(.),
  http_redirect(moved, location_by_id(home_page)),
  []).

:- http_handler(
  root(home),
  home_page,
  []).

home_page(_Request) :-
  % findall gathers all the solutions from the 2nd argument query,
  % transforms them with the first argument,
  % and places the resulting list in the 3rd argument.
  findall(h2(P), grandfather_of(richard, P), L),

  Title = 'Grandchildren of Richard',
  reply_html_page(
    title(Title),
    [h1(Title) | L] % page body
  ).
