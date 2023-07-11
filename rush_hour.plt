% To run these tests, enter `swipl rush_hour.plt`

% Suppresss warnings about singleton varibles.
:-style_check(-singleton).

:- consult(rush_hour).

:- begin_tests(rush_hour).

% Suppress warnings about unused variables.
:- style_check(-singleton).

test(board_row_string) :-
  board_row_string(['A', 'B', 'C', 'D', 'E', 'F'], S),
  assertion(S == "|A B C D E F|").

test(board_string) :-
  Board = [
    [a, a,   ' ', ' ', ' ', o  ],
    [p, ' ', ' ', q,   ' ', o  ],
    [p, x,   x,   q,   ' ', o  ],
    [p, ' ', ' ', q,   ' ', ' '],
    [b, ' ', ' ', ' ', c,   c  ],
    [b, ' ', r,   r,   r,   ' ']
  ],
  Expected = "+-----------+\n|a a       o|\n|p     q   o|\n|p x x q   o|\n|p     q    |\n|b       c c|\n|b   r r r  |\n+-----------+\n",
  board_string(Board, S),
  assertion(S == Expected).

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

test(repeat) :-
  repeat(x, 0, ""),
  repeat(x, 1, "x"),
  repeat(x, 3, "xxx").

:- end_tests(rush_hour).
:- run_tests.
:- halt.
