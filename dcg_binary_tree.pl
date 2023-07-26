:- use_module(library(dcgs)).

nodes(nil) --> [].
nodes(node(Node, L, R)) --> [Node], nodes(L), nodes(R).

:- initialization((
  phrase(
    nodes(
      node(
        a,
        nil,
        node(
          b,
          node(c, nil, nil),
          nil
        )
      )
    ),
    L
  ),
  write(L),
  nl
  % output is list of leaf node values [a,b,c].
)).
