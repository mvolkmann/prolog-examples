print_row(L) :-
  % The 1st list element is left-aligned.
  % The 2nd list element is center-aligned.
  % The 3rd list element is right-aligned.
  format("~w~t~10+~t~w~t~10+~t~w~10+~n", L).

:- print_row(["foo", "bar", "baz"]),
   print_row(["folish", "barking", "bazooka"]).

/* The output is:
foo          bar           baz
folish     barking     bazooka
*/
