% The default value for the double_quotes flag is string
% which is what we need for this example.
% :- set_prolog_flag(double_quotes, string).

print_row(Row) :-
  % The 1st list element is left-aligned.
  % The 2nd list element is center-aligned.
  % The 3rd list element is right-aligned.
  format('~w~t~10+~t~w~t~10+~t~w~10+~n', Row).

:- initialization((
  Rows = [
    ["foo", "bar", "baz"],
    ["foolish", "barking", "bazooka"]
  ],
  maplist(print_row, Rows),
  halt
)).

/* The output is:
foo          bar           baz
foolish     barking     bazooka
*/
