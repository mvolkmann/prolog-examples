print_row(Row) :-
  % The 1st list element is left-aligned.
  % The 2nd list element is center-aligned.
  % The 3rd list element is right-aligned.
  format("~w~t~10+~t~w~t~10+~t~w~10+~n", Row).

:- Rows = [
     ["foo", "bar", "baz"],
     ["foolish", "barking", "bazooka"]
   ],
   maplist(print_row, Rows).

/* The output is:
foo          bar           baz
foolish     barking     bazooka
*/
