:- module(html_gen, [
    a/4, body/3, dv/3, h1/3, h2/3, head/3, html/4, li/3, style/3, title/3, ul/3
  ]).

:- use_module(library(dcgs)).

tag1(Name, Content) --> "<", Name, ">", Content, "</", Name, ">".
tag2(Name, Children) --> "<", Name, ">", children(Children), "</", Name, ">".

children([]) --> [].
children([H|T]) --> H, children(T).

a(URL, Text) --> "<a href=\"", URL, "\">", Text, "</a>".
body(Content) --> tag2("body", Content).
% div is the name of a builtin predicate,
% so I'm naming this dv to avoid conflict.
dv(Content) --> "<h1>", Content, "</h1>".
h1(Content) --> tag1("h1", Content).
h2(Content) --> tag1("h2", Content).
head(Content) --> tag2("head", Content).
html(Head, Body) --> "<html>", Head, Body, "</html>".
li(Content) --> tag1("li", Content).
style(Content) --> tag2("style", Content).
title(Content) --> tag1("title", Content).
ul(Content) --> tag2("ul", Content).
