% This prints all the values found in a linked list.
print_list(nil) :- !.
print_list(Node) :-
  writeln(Node.value),
  print_list(Node.next).

% This relates a node in a linked list to
% a list of values found in all reachable nodes.
linked_list(nil, []).
linked_list(Node, L) :-
  linked_list(Node.next, L2),
  % This appends in the order encounter.
  % append([Node.value], L2, L).
  % This appends in reverse order.
  append(L2, [Node.value], L).

:- initialization((
  N1 = node{value: 'alpha', next: nil},
  N2 = node{value: 'beta', next: N1},
  N3 = node{value: 'gamma', next: N2},

  print_list(N3), % prints gamma then beta then alpha

  linked_list(N3, L), % [alpha, beta, gamma]

  % This creates a string containing joined values from a list.
  atomics_to_string(L, ',', S),

  writeln(S), % alpha,beta,gamma
  halt
)).
