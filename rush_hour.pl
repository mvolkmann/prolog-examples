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
:- include(util). % textually includes the contents of the file util.pl

% Constants
exit_row(2).
size(6).

% You don't need a rule to make a deep copy of a board nested list.
% Just use the copy_term(In, Out) predicate.

% TODO: FINISH THIS!
% add_horizontal_moves() :-

% TODO: FINISH THIS!
% add_vertical_moves() :-

% TODO: Finish this (see getBoard in rush_hour.js).
% board(Positions) :-

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
    p1: puzzle{
      a: car{ fixed: 0, variable: 0, horizontal: true },
      b: car{ fixed: 0, variable: 4 },
      c: car{ fixed: 4, variable: 4, horizontal: true },
      o: car{ fixed: 5, variable: 0 },
      p: car{ fixed: 0, variable: 1 },
      q: car{ fixed: 3, variable: 1 },
      r: car{ fixed: 5, variable: 2, horizontal: true },
      x: car{ fixed: EXIT_ROW, variable: 1, horizontal: true }
    },
    p30: puzzle{
      a: car{ fixed: 2, variable: 0 },
      b: car{ fixed: 3, variable: 1 },
      c: car{ fixed: 3, variable: 0, horizontal: true },
      d: car{ fixed: 3, variable: 2, horizontal: true },
      e: car{ fixed: 5, variable: 0, horizontal: true },
      f: car{ fixed: 5, variable: 2, horizontal: true },
      o: car{ fixed: 0, variable: 0 },
      p: car{ fixed: 0, variable: 3, horizontal: true },
      q: car{ fixed: 5, variable: 3 },
      x: car{ fixed: EXIT_ROW, variable: 1, horizontal: true }
    },
    p40: puzzle{
      a: car{ fixed: 0, variable: 1, horizontal: true },
      b: car{ fixed: 4, variable: 0 },
      c: car{ fixed: 1, variable: 1 },
      d: car{ fixed: 2, variable: 1 },
      e: car{ fixed: 3, variable: 3 },
      f: car{ fixed: 2, variable: 4 },
      g: car{ fixed: 4, variable: 4, horizontal: true },
      h: car{ fixed: 5, variable: 0, horizontal: true },
      i: car{ fixed: 5, variable: 3, horizontal: true },
      o: car{ fixed: 0, variable: 0 },
      p: car{ fixed: 5, variable: 1 },
      q: car{ fixed: 3, variable: 0, horizontal: true },
      x: car{ fixed: EXIT_ROW, variable: 3, horizontal: true }
    }
  }.

% This sets the board letter used in a range of rows for a given column.
set_column(Board, _, _, _, 0, Board) :- !.
set_column(Board, Letter, Column, StartRow, Length, NewBoard) :-
  format('set_column: Letter = ~w~n', [Letter]),
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
  format('set_row: Letter=~w, Length=~w~n', [Letter, Length]),
  nth0(Row, Board, BoardRow),
  replace(BoardRow, StartColumn, Letter, NewBoardRow),
  copy_term(Board, Board2),
  replace(Board2, Row, NewBoardRow, Board3),
  S is StartColumn + 1,
  L is Length - 1,
  set_row(Board3, Letter, Row, S, L, NewBoard).

add_car(Puzzle, Board, Letter, NewBoard) :-
  format('add_car: Letter = ~w~n', [Letter]),
  Car = Puzzle.get(Letter),
  format('add_car: Car = ~w~n', [Car]),
  letter_index(Letter, Index),
  format('add_car: Index = ~w~n', [Index]),
  car_length(Index, Length),
  format('add_car: Length = ~w~n', [Length]),
  H = Car.get(horizontal, false),
  format('add_car: H = ~w~n', [H]),
  format('add_car: fixed = ~w~n', [Car.fixed]),
  format('add_car: variable = ~w~n', [Car.variable]),
  H == true ->
    set_row(Board, Letter, Car.fixed, Car.variable, Length, NewBoard);
    set_column(Board, Letter, Car.fixed, Car.variable, Length, NewBoard).

solve(Puzzle) :-
  (X = Puzzle.get(x) ->
    writeln('Found car x.');
    writeln('Puzzle is missing car X!')
  ),
  empty_board(Board),
  dict_keys(Puzzle, Keys),
  foldl(add_car(Puzzle), Keys, Board, NewBoard).
  % TODO: CONTINUE ADDING CODE HERE!

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

write_board_row(Stream, Row) :-
  /* For now I'm hard-coding to format string to make this faster.
  format(Stream, '|', []),
  size(Size),
  fill(Size, '~w', Parts),
  atomics_to_string(Parts, ' ', Format),
  */
  format(Stream, '|~w ~w ~w ~w ~w ~w|\n', Row).
  % format(Stream, '|\n', []).

write_board_rows(_, []) :- !.
write_board_rows(Stream, [H|T]) :-
  write_board_row(Stream, H),
  write_board_rows(Stream, T).

write_board(Stream, Board) :-
  border(Border),
  writeln(Stream, Border),
  write_board_rows(Stream, Board),
  writeln(Stream, Border).

/*
car_at(X, Y, car(C, (X1, Y1), (X2, Y2))) :-
  between(X1, X2, X),
  between(Y1, Y2, Y).

car_at(X, Y, [H|T]) :-
  car_at(X, Y, H);
  car_at(X, Y, T).
  
board_character(Row, Column, Cars, Char) :-
  car_at(Row, Column, Cars) -> 

% This relates a current board (CB) and a car to a new board (NB).
% TODO: TEST THIS!
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

:- initialization
  % halt.
*/
