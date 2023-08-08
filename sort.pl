/*
- variables sorted by their names
- floating point numbers from lowest to highest
- integers from lowest to highest
- atoms sorted by their names
- compound terms (structures) sorted by their arity and then by their functor name
*/

demo :-
  L0 = [b(a1, a2), b(a1), a(a1, a2), a(a1), foo, bar, 2, 1, 2.2, 1.1, B, A],
  sort(L0, L),
  write(L).
