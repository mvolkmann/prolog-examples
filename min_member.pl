younger(person(_, A1), person(_, A2)) :- A1 < A2.

age_compare(>, person(_, A1), person(_, A2)) :- A1 > A2.
age_compare(<, person(_, A1), person(_, A2)) :- A1 < A2.
age_compare(=, person(_, A1), person(_, A2)) :- A1 = A2.

?- initialization
  P1 = person(ann, 35),
  P2 = person(bob, 50),
  P3 = person(carl, 19),
  People = [P1, P2, P3],

  min_member(younger, Py, People),
  person(N1, A1) = Py,
  format("youngest is ~w at age ~w~n", [N1, A1]),

  max_member(younger, Po, People),
  person(N2, A2) = Po,
  format("oldest is ~w at age ~w~n", [N2, A2]),

  predsort(age_compare, People, Sorted),
  writeln(Sorted),
  halt.
  % output is [person(carl,19),person(ann,35),person(bob,50)]

