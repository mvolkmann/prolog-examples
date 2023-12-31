% To run these tests, enter `swipl rush_hour.plt`

% Suppress warnings about singleton variables.
:-style_check(-singleton).

:- consult(rush_hour).

:- begin_tests(rush_hour).

test(car_length) :-
  letter_index(a, IndexA),
  car_length(IndexA, LengthA),
  assertion(LengthA == 2),
  letter_index(r, IndexR),
  car_length(IndexR, LengthR),
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

test(goal_reached) :-
  B1 = [[], [], [x, x, ' ', ' ', ' ', ' '], [], [], []],
  goal_reached(B1), % reached
  B2 = [[], [], [x, x, ' ', 'a', ' ', ' '], [], [], []],
  \+ goal_reached(B2). % not reached

test(index_letter) :-
  index_letter(0, a),
  index_letter(1, b).

test(letter_index) :-
  letter_index(a, 0),
  letter_index(b, 1).

test(moves) :-
  State0 = state{
    move: nil,
    previousState: nil
  },
  State1 = state{
    move: 'A left 1',
    previousState: State0
  },
  State2 = state{
    move: 'B down 3',
    previousState: State1
  },
  moves(State2, Moves),
  % TODO: I can't find a way to ignore the last node when its move is nil!
  assertion(Moves == [nil, 'A left 1', 'B down 3']).

test(pending_state_added) :-
  OldStates = [],
  NewState = state{
    board: 'some value',
    variablePositions: [],
    move: 'some move',
    previousState: []
  },
  pending_state_added(NewState, OldStates, NewStates),
  length(NewStates, 1),
  [H|T] = NewStates,
  assertion(H == NewState),
  length(T, 0).

test(set_column) :-
  Board = [
    [],
    [' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' '],
    [],
    [],
    []
  ],
  Column = 2,
  set_column(Board, x, Column, 1, 2, NewBoard),
  Expected = [
    [],
    [' ', ' ', x, ' ', ' ', ' '],
    [' ', ' ', x, ' ', ' ', ' '],
    [],
    [],
    []
  ],
  assertion(NewBoard =@= Expected).

test(set_row) :-
  Board = [[], [], [' ', ' ', ' ', ' ', ' ', ' '], [], [], []],
  Row = 2,
  set_row(Board, x, Row, 1, 2, NewBoard),
  nth0(Row, NewBoard, BoardRow),
  assertion(BoardRow =@= [' ', x, x, ' ', ' ', ' ']).

test(solve) :-
  puzzles(P),
  solve(P.p1).

test(space_down) :-
  Board = [
    [' ', ' ', a,   ' ', ' ', ' '],
    [' ', ' ', b,   ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', x,   ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ']
  ],
  Row = 1,
  Column = 2,
  space_down(Board, Column, Row, Space),
  assertion(Space == 2).

test(space_left) :-
  Board = [
    [],
    [a, b, ' ', ' ', x, ' '],
    [],
    [],
    [],
    []
  ],
  Row = 1,
  Column = 4,
  space_left(Board, Row, Column, Space),
  assertion(Space == 2).

test(space_right) :-
  Board = [
    [],
    [a, b, ' ', ' ', x, ' '],
    [],
    [],
    [],
    []
  ],
  Row = 1,
  Column = 1,
  space_right(Board, Row, Column, Space),
  assertion(Space == 2).

test(space_up) :-
  Board = [
    [' ', ' ', a,   ' ', ' ', ' '],
    [' ', ' ', b,   ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', x,   ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ']
  ],
  Row = 4,
  Column = 2,
  space_up(Board, Column, Row, Space),
  assertion(Space == 2).

test(state_id) :-
  Positions = [1, 2, [], 3, [], [], 4, []],
  state_id(Positions, Id),
  assertion(Id == "1234").

test(write_board_row) :-
  new_memory_file(Handle),
  open_memory_file(Handle, write, Stream, [free_on_close(true)]),
  write_board_row(Stream, ['A', 'B', 'C', 'D', 'E', 'F']),
  close(Stream),
  memory_file_to_string(Handle, S),
  assertion(S == "|A B C D E F|\n").

test(write_board) :-
  Board = [
    [a, a,   ' ', ' ', ' ', o  ],
    [p, ' ', ' ', q,   ' ', o  ],
    [p, x,   x,   q,   ' ', o  ],
    [p, ' ', ' ', q,   ' ', ' '],
    [b, ' ', ' ', ' ', c,   c  ],
    [b, ' ', r,   r,   r,   ' ']
  ],
  new_memory_file(Handle),
  open_memory_file(Handle, write, Stream, [free_on_close(true)]),
  write_board(Stream, Board),
  close(Stream),
  memory_file_to_string(Handle, S),
  Expected = "+-----------+\n|a a       o|\n|p     q   o|\n|p x x q   o|\n|p     q    |\n|b       c c|\n|b   r r r  |\n+-----------+\n",
  assertion(S == Expected).

:- end_tests(rush_hour).
:- run_tests.
:- halt.
