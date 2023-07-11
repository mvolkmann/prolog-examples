% To run these tests, enter `swipl rush_hour.plt`

:- consult(rush_hour).
:- begin_tests(rush_hour).

% Suppress warnings about unused variables.
:- style_check(-singleton).

test(board_row_string) :-
  board_row_string(['A', 'B', 'C', 'D', 'E', 'F'], S),
  assertion(S == "|A B C D E F|\n").

test(car_length) :-
  car_length(a, LengthA),
  assertion(LengthA == 2),
  car_length(r, LengthR),
  assertion(LengthR == 3).

validate_board_row(Row) :-
  size(Size),
  length(Row, Size),
  maplist(=(' '), Row).

test(empty_board_row) :-
  empty_board_row(Row),
  validate_board_row(Row).

test(empty_board) :-
  empty_board(Board),
  size(Size),
  length(Board, Size),
  maplist(validate_board_row, Board).

:- end_tests(rush_hour).
:- run_tests.
:- halt.
