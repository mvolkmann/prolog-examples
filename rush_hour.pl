/*
This finds solutions to Rush Hour puzzles.
The board is a 6x6 grid.
The cars have a color, start position, and end position.
Each position is represented by an (X, Y) structure.
X values go left to right from 0 to 5.
Y values go top to bottom from 0 to 5.
Each car is represented by a car(Color, Start, End) structure.
Each color is a single letter code so the board can be printed nicely.
*/

:- set_prolog_flag(double_quotes, chars).

/*
car_at(X, Y, car(C, (X1, Y1), (X2, Y2))) :-
  between(X1, X2, X),
  between(Y1, Y2, Y).

car_at(X, Y, [H|T]) :-
  car_at(X, Y, H);
  car_at(X, Y, T).
  
board_character(Row, Column, Cars, Char) :-
  car_at(Row, Column, Cars) -> 
*/

% Constants
exit_row(2).
size(6).

% You don't need a rule to make a deep copy of a board nested list.
% Just use the copy_term(In, Out) predicate.

% TODO: Finish this (getBoard in rush_hour.js).
% board(Positions) :-

% TODO: FINISH THIS!
% add_horizontal_moves() :-

% Gets a border string used when printing a board.
border(B) :-
  size(Size),
  Count is Size * 2 - 1,
  repeat('-', Count, Dashes),
  atomics_to_string(['+', Dashes, '+'], B).

car_length(Index, Length) :-
  letter_index(o, IndexO),
  letter_index(r, IndexR),
  between(IndexO, IndexR, Index) -> Length = 3; Length = 2.

% Gets a column of values from a 2D list.
column(_, [], []) :- !.
column(N, Board, Column) :-
  [Row|Rest] = Board,
  nth0(N, Row, Element),
  column(N, Rest, Column2),
  Column = [Element | Column2].

empty_board_row(Row) :-
  size(Size),
  length(Row, Size),
  maplist(=(' '), Row).

empty_board_row(_, Row) :-
  empty_board_row(Row).

empty_board(Board) :-
  size(Size),
  length(Rows, Size),
  maplist(empty_board_row, Rows, Board).

% This creates a list containing N copies of E.
fill(0, _, []) :- !.
fill(N, E, L) :-
  N2 is N - 1,
  fill(N2, E, L2),
  L = [E | L2].

% This determines if there are any cars blocking the x car exit.
goal_reached(Board) :-
  nth0(2, Board, Row2),
  tail_after_last(x, Row2, T),
  maplist(=(' '), T).

index_letter(I, L) :-
  char_code(a, CodeA),
  CodeL is I + CodeA,
  char_code(L, CodeL).

letter_index(L, I) :-
  char_code(L, CodeL),
  char_code(a, CodeA),
  I is CodeL - CodeA.

moves(nil, []) :- !.
moves(State, L) :-
  moves(State.previousState, L2),
  Move = State.get(move, ''),
  append(L2, [Move], L).

pending_state_added(NewState, OldStates, NewStates) :-
  append(OldStates, [NewState], NewStates).

print_board(Board) :-
  board_string(Board, S),
  write(S).

print_moves(State) :-
  moves_string(State, S),
  write(S).

puzzles(P) :-
  P = puzzles{
    p1: {
      A: { fixed: 0, variable: 0, horizontal: true },
      B: { fixed: 0, variable: 4 },
      C: { fixed: 4, variable: 4, horizontal: true },
      O: { fixed: 5, variable: 0 },
      P: { fixed: 0, variable: 1 },
      Q: { fixed: 3, variable: 1 },
      R: { fixed: 5, variable: 2, horizontal: true },
      X: { fixed: EXIT_ROW, variable: 1, horizontal: true }
    },
    p30: {
      A: { fixed: 2, variable: 0 },
      B: { fixed: 3, variable: 1 },
      C: { fixed: 3, variable: 0, horizontal: true },
      D: { fixed: 3, variable: 2, horizontal: true },
      E: { fixed: 5, variable: 0, horizontal: true },
      F: { fixed: 5, variable: 2, horizontal: true },
      O: { fixed: 0, variable: 0 },
      P: { fixed: 0, variable: 3, horizontal: true },
      Q: { fixed: 5, variable: 3 },
      X: { fixed: EXIT_ROW, variable: 1, horizontal: true }
    },
    p40: {
      A: { fixed: 0, variable: 1, horizontal: true },
      B: { fixed: 4, variable: 0 },
      C: { fixed: 1, variable: 1 },
      D: { fixed: 2, variable: 1 },
      E: { fixed: 3, variable: 3 },
      F: { fixed: 2, variable: 4 },
      G: { fixed: 4, variable: 4, horizontal: true },
      H: { fixed: 5, variable: 0, horizontal: true },
      I: { fixed: 5, variable: 3, horizontal: true },
      O: { fixed: 0, variable: 0 },
      P: { fixed: 5, variable: 1 },
      Q: { fixed: 3, variable: 0, horizontal: true },
      X: { fixed: EXIT_ROW, variable: 3, horizontal: true }
    }
  }.

repeat_(_, 0, []) :- !.
repeat_(Char, N, [Char|T]) :-
  N2 is N - 1,
  repeat_(Char, N2, T).
% repeat("*", 3, S). 

% The first two arguments must be instantiated (ground).
repeat(Char, N, S) :-
  ground(Char),
  ground(N),
  repeat_(Char, N, L),
  atomics_to_string(L, S).

% This replaces a list element at a given zero-based index.
% For example, replace([a, b, c], 1, d, L) sets L to [a, d, c].
replace([], _, _, []).
replace([_|T], 0, X, [X|T]) :- !.
replace([H|T], I, X, [H|R]):- I > 0, I1 is I-1, replace(T, I1, X, R).

% This sets the board letter used in a range of rows for a given column.
set_column(Board, _, _, _, 0, Board) :- !.
set_column(Board, Letter, Column, StartRow, Length, NewBoard) :-
  nth0(StartRow, Board, BoardRow),
  replace(BoardRow, Column, Letter, NewBoardRow),
  copy_term(Board, Board2),
  replace(Board2, StartRow, NewBoardRow, Board3),
  S is StartRow + 1,
  L is Length - 1,
  set_column(Board3, Letter, Column, S, L, NewBoard).

% This sets the board letter used in a range of columns for a given row.
% If the length is zero, the board remains unchanged.
set_row(Board, _, _, _, 0, Board) :- !.
set_row(Board, Letter, Row, StartColumn, Length, NewBoard) :-
  nth0(Row, Board, BoardRow),
  replace(BoardRow, StartColumn, Letter, NewBoardRow),
  copy_term(Board, Board2),
  replace(Board2, Row, NewBoardRow, Board3),
  S is StartColumn + 1,
  L is Length - 1,
  set_row(Board3, Letter, Row, S, L, NewBoard).

% Gets number of empty spaces to left of a given board row column.
space_row_left(BoardRow, 0, 0).
space_row_left(BoardRow, Column, Space) :-
  Left is Column - 1,
  nth0(Left, BoardRow, Value),
  Value =@= ' ' ->
    space_row_left(BoardRow, Left, S), Space is 1 + S;
    Space is 0.

% Gets number of empty spaces to right of a given board row column.
space_row_right(BoardRow, 0, 0).
space_row_right(BoardRow, Column, Space) :-
  Right is Column + 1,
  nth0(Right, BoardRow, Value),
  Value =@= ' ' ->
    space_row_right(BoardRow, Right, S), Space is 1 + S;
    Space is 0.

% Gets number of empty spaces below a given board cell.
space_down(Board, Row, Column, Space) :-
  % Slice is a list of values in Column.
  column(Column, Board, Slice),
  space_row_right(Slice, Row, Space).

% Gets number of empty spaces to left of a given board cell.
space_left(Board, Row, Column, Space) :-
  nth0(Row, Board, BoardRow),
  space_row_left(BoardRow, Column, Space).

% Gets number of empty spaces to right of a given board cell.
space_right(Board, Row, Column, Space) :-
  nth0(Row, Board, BoardRow),
  space_row_right(BoardRow, Column, Space).

% Gets number of empty spaces above a given board cell.
space_up(Board, Row, Column, Space) :-
  % Slice is a list of values in Column.
  column(Column, Board, Slice),
  space_row_left(Slice, Row, Space).

% positions.filter(p => p !== null).join('');
state_id(Positions, Id) :-
  exclude(=([]), Positions, Used),
  atomics_to_string(Used, Id).

% This gets the tail of a list that follows
% the last occurrence of a given element.
tail_after_last(_, [], []) :- !.
tail_after_last(E, L, A) :-
  member(E, L) ->
    [_|T] = L, tail_after_last(E, T, A);
    A = L.

write_board_row(Stream, Row) :-
  format(Stream, '|', []),
  size(Size),
  fill(Size, '~w', Parts),
  atomics_to_string(Parts, ' ', Format),
  format(Stream, Format, Row),
  format(Stream, '|\n', []).

write_board_rows(_, []) :- !.
write_board_rows(Stream, [H|T]) :-
  format('H = ~w~n', [H]),
  write_board_row(Stream, H),
  format('T = ~w~n', [T]),
  write_board_rows(Stream, T).

write_board(Board, Stream) :-
  border(Border),
  format(Stream, Border, []),
  write_board_rows(Board, Stream),
  format(Stream, Border, []).

/*
% This relates a car (C) to whether it is horizontal (H).
horizontal_car(C, H) :-
  _ = C.get(row) -> H = true; H = false.

% This relates a current board (CB) and a car to a new board (NB).
% TODO: TEST THIS!
% TODO: Still need to write set_column and set_row.
car_board(CB, Letter, Car, NB) :-
  car_length(Car, Length),
  horizontal_car(Car, H),
  H -> 
    % Handle horizontal car.
    Row = Car.get(row),
    StartColumn = Car.get(currentColumn),
    set_row(CB, Letter, StartColumn, Length, NB);
    % Handle vertical car.
    Column = Car.get(column),
    StartRow = Car.get(currentRow),
    set_column(CB, Letter, StartRow, Length, NB).

% This creates a 2D array of car letters for a given puzzle.
cars_board(Cars, Board) :-
  (X = Cars.get(x) ->
    writeln('Found car x.');
    writeln("Puzzle is missing car X!"),halt
  ),
  empty_board(Board).

% This is equivalent to the setColumn function in rush_hour.js.
column_board(CB, Letter, Column, StartRow, Length, NB) :-
  TODO: Finish this.

% This is equivalent to the setRow function in rush_hour.js.
row_board(CB, Letter, Row, StartColumn, Length, NB) :-
  TODO: Finish this.

  const boardRows = [];

  // Create an empty board.
  for (let row = 0; row < SIZE; row++) {
    const boardRow = Array(SIZE).fill(SPACE);
    boardRows.push(boardRow);
  }

  // Add cars to the board.
  for (const letter of letters) {
    const car = cars[letter];
    const length = carLength(letter);

    if (isHorizontal(car)) {
      const start = car.currentColumn;
      const end = start + length;
      const boardRow = boardRows[car.row];
      for (let column = start; column < end; column++) {
        // Check if another car already occupies this cell.
        // If so then there is a error in the puzzle description.
        const existing = boardRow[column];
        if (existing !== SPACE) {
          console.error(`Car ${letter} overlaps car {existing}!`);
          process.exit(3);
        }

        boardRow[column] = letter;
      }
    } else {
      // The car is vertical.
      const { column } = car;
      const start = car.currentRow;
      const end = start + length;
      for (let row = start; row < end; row++) {
        const boardRow = boardRows[row];

        // Check if another car already occupies this cell.
        // If so then there is a error in the puzzle description.
        const existing = boardRow[column];
        if (existing !== SPACE) {
          console.error(`Car ${letter} overlaps car {existing}!`);
          process.exit(3);
        }

        boardRow[column] = letter;
      }
    }
  }

  return boardRows;

% print(board(Size, Size, Exit, Cars)) :-

:- initialization
  Puzzles = puzzleDict{
    p1: cars{
      a: car{ row: 0, currentColumn: 0 },
      b: car{ column: 0, currentRow: 4 },
      c: car{ row: 4, currentColumn: 4 },
      o: car{ column: 5, currentRow: 0 },
      p: car{ column: 0, currentRow: 1 },
      q: car{ column: 3, currentRow: 1 },
      r: car{ row: 5, currentColumn: 2 },
      x: car{ row: ExitRow, currentColumn: 1 }
    },
    p30: cars{
      a: car{ column: 2, currentRow: 0 },
      b: car{ column: 3, currentRow: 1 },
      c: car{ row: 3, currentColumn: 0 },
      d: car{ row: 3, currentColumn: 2 },
      e: car{ row: 5, currentColumn: 0 },
      f: car{ row: 5, currentColumn: 2 },
      o: car{ column: 0, currentRow: 0 },
      p: car{ row: 0, currentColumn: 3 },
      q: car{ column: 5, currentRow: 3 },
      x: car{ row: ExitRow, currentColumn: 1 }
    },
    p40: cars{
      a: car{ row: 0, currentColumn: 1 },
      b: car{ column: 4, currentRow: 0 },
      c: car{ column: 1, currentRow: 1 },
      d: car{ column: 2, currentRow: 1 },
      e: car{ column: 3, currentRow: 3 },
      f: car{ column: 2, currentRow: 4 },
      g: car{ row: 4, currentColumn: 4 },
      h: car{ row: 5, currentColumn: 0 },
      i: car{ row: 5, currentColumn: 3 },
      o: car{ column: 0, currentRow: 0 },
      p: car{ column: 5, currentRow: 1 },
      q: car{ row: 3, currentColumn: 0 },
      x: car{ row: ExitRow, currentColumn: 3 }
    }
  },

  Board = [
    ['A', ' ', ' ', ' ', ' ', ' '],
    [' ', 'B', ' ', ' ', ' ', ' '],
    [' ', ' ', 'C', ' ', ' ', ' '],
    [' ', ' ', ' ', 'D', ' ', ' '],
    [' ', ' ', ' ', ' ', 'E', ' '],
    [' ', ' ', ' ', ' ', ' ', 'F']
  ],
  
  cars_board(Puzzles.p1, Board),
  printBoard(Board).
  % halt.
*/
