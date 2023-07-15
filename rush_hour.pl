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

add_car(Puzzle, Board, Letter, NewBoard) :-
  Car = Puzzle.get(Letter),
  letter_index(Letter, Index),
  car_length(Index, Length),
  H = Car.get(horizontal, false),
  (get_dict(horizontal, Car, H) ->
    set_row(Board, Letter, Car.fixed, Car.variable, Length, NewBoard);
    set_column(Board, Letter, Car.fixed, Car.variable, Length, NewBoard)
  ).

add_moves(Cars, Board, Letter) :-
  format('=> addMoves: Letter=~w~n', [Letter]),
  Car = Cars.get(Letter),
  H = Car.get(horizontal, false),
  (get_dict(horizontal, Car, H) ->
    % For horizontal cars ...
    moves_left(Board, Cars, Letter),
    moves_right(Board, Cars, Letter);

    % For vertical cars ...
    format('add_moves: before moves_up, Car ~w is ~w~n', [Letter, Car]),
    moves_up(Board, Cars, Letter),
    Car2 = Cars.get(Letter),
    format('add_moves: after moves_up, Car ~w is ~w~n', [Letter, Car2]),
    moves_down(Board, Cars, Letter),
    Car3 = Cars.get(Letter),
    format('add_moves: after moves_down, Car ~w is ~w~n', [Letter, Car3])
  ).

% THIS SHOULD NOT MODIFY Cars! Does it?
add_horizontal_moves(Cars, Board, Letter, Row, StartColumn, A, B, Delta) :-
  format('~w moves to column ~w~n', [Letter, A]),

  copy_term(Cars.get(Letter), NewCar),
  nb_set_dict(variable, NewCar, A),
  copy_term(Cars, NewCars),
  nb_set_dict(Letter, NewCars, NewCar),

  letter_index(Letter, Index),
  car_length(Index, Length),
  set_row(Board, ' ', Row, StartColumn, Length, Board2),
  set_row(Board2, Letter, Row, A, Length, Board3),
  writeln('add_horizontal_moves: Board3 follows'),
  print_board(Board3),

  nb_getval(pendingStates, PendingStates),
  NewState = [Board3, NewCars],
  append(PendingStates, [NewState], NewPendingStates),
  nb_setval(pendingStates, NewPendingStates),

  (A =\= B ->
    Next #= A + Delta,
    add_horizontal_moves(Cars, Board, Letter, Row, StartColumn, Next, B, Delta);
    true
  ).

% THIS SHOULD NOT MODIFY Cars! Does it?
add_vertical_moves(Cars, Board, Letter, Column, StartRow, A, B, Delta) :-
  format('~w moves to row ~w~n', [Letter, A]),

  copy_term(Cars.get(Letter), NewCar),
  nb_set_dict(variable, NewCar, A),
  copy_term(Cars, NewCars),
  nb_set_dict(Letter, NewCars, NewCar),

  letter_index(Letter, Index),
  car_length(Index, Length),
  set_column(Board, ' ', Column, StartRow, Length, Board2),
  set_column(Board2, Letter, Column, A, Length, Board3),
  writeln('add_vertical_moves: Board3 follows'),
  print_board(Board3),

  nb_getval(pendingStates, PendingStates),
  NewState = [Board3, NewCars],
  append(PendingStates, [NewState], NewPendingStates),
  nb_setval(pendingStates, NewPendingStates),

  (A =\= B ->
    Next #= A + Delta,
    add_vertical_moves(Cars, Board, Letter, Column, StartRow, Next, B, Delta);
    true
  ).

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

moves_down(Board, Cars, Letter) :-
  format('== moves_down: Letter=~w~n', [Letter]),
  Car = Cars.get(Letter),
  Column = Car.fixed,
  format('moves_down: Column=~w~n', [Column]),
  StartRow = Car.variable,
  format('moves_down: StartRow=~w~n', [StartRow]),
  letter_index(Letter, Index),
  car_length(Index, Length),
  format('moves_down: Length=~w~n', [Length]),
  EndRow #= StartRow + Length - 1,
  format('moves_down: EndRow=~w~n', [EndRow]),
  space_down(Board, Column, EndRow, Space),
  format('** moves_down: space below ~w is ~w~n', [Letter, Space]),
  (Space #> 0 ->
    A #= StartRow + Space,
    B #= StartRow + 1,
    add_vertical_moves(Cars, Board, Letter, Column, StartRow, A, B, 1);
    true
  ).

moves_left(Board, Cars, Letter) :-
  Car = Cars.get(Letter),
  Row = Car.fixed,
  StartColumn = Car.variable,
  space_left(Board, Row, StartColumn, Space),
  (Space #> 0 ->
    A #= StartColumn - Space,
    B #= StartColumn - 1,
    format('moves_left: moving ~w left ~w to ~w~n', [Letter, A, B]),
    add_horizontal_moves(Cars, Board, Letter, Row, StartColumn, A, B, 1);
    true
  ).

moves_right(Board, Cars, Letter) :-
  Car = Cars.get(Letter),
  Row = Car.fixed,
  StartColumn = Car.variable,
  letter_index(Letter, Index),
  car_length(Index, Length),
  EndColumn #= StartColumn + Length - 1,
  space_right(Board, Row, EndColumn, Space),
  format('** moves_right: space to right of ~w is ~w~n', [Letter, Space]),
  (Space #> 0 ->
    format('moves_right: StartColumn = ~w~n', [StartColumn]),
    format('moves_right: Length = ~w~n', [Length]),
    format('moves_right: EndColumn = ~w~n', [EndColumn]),
    format('moves_right: board follows\n'),
    print_board(Board),
    A #= StartColumn + Space,
    B #= StartColumn + 1,
    format('moves_right: moving ~w right ~w to ~w~n', [Letter, A, B]),
    add_horizontal_moves(Cars, Board, Letter, Row, StartColumn, A, B, -1);
    true
  ).

moves_up(Board, Cars, Letter) :-
  Car = Cars.get(Letter),
  Column = Car.fixed,
  StartRow = Car.variable,
  space_up(Board, Column, StartRow, Space),
  (Space #> 0 ->
    A #= StartRow - Space,
    B #= StartRow - 1,
    add_vertical_moves(Cars, Board, Letter, Column, StartRow, A, B, 1);
    true
  ).

pending_state_added(NewState, OldStates, NewStates) :-
  append(OldStates, [NewState], NewStates).

print_board(Board) :- write_board(user_output, Board).

/*
print_moves(State) :-
  moves_string(State, S),
  write(S).
*/

print_state([Board, Cars]) :- print_board(Board).

puzzles(P) :-
  exit_row(ExitRow),
  P = puzzles{
    p1: puzzle{
      a: car{ fixed: 0, variable: 0, horizontal: true },
      b: car{ fixed: 0, variable: 4 },
      c: car{ fixed: 4, variable: 4, horizontal: true },
      o: car{ fixed: 5, variable: 0 },
      p: car{ fixed: 0, variable: 1 },
      q: car{ fixed: 3, variable: 1 },
      r: car{ fixed: 5, variable: 2, horizontal: true },
      x: car{ fixed: ExitRow, variable: 1, horizontal: true }
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
      x: car{ fixed: ExitRow, variable: 1, horizontal: true }
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
      x: car{ fixed: ExitRow, variable: 3, horizontal: true }
    }
  }.

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

solve(Puzzle) :-
  (_ = Puzzle.get(x) -> true; writeln('Puzzle is missing car X!')),
  empty_board(Board),
  dict_keys(Puzzle, Letters),
  foldl(add_car(Puzzle), Letters, Board, NewBoard),
  writeln('Starting board:'),
  print_board(NewBoard),

  InitialState = [Board, Puzzle],
  nb_setval(pendingStates, [InitialState]),

  maplist(add_moves(Puzzle, NewBoard), Letters),
  nb_getval(pendingStates, PendingStates),
  format('PendingStates = ~w~n', [PendingStates]),
  maplist(print_state, PendingStates).
  % TODO: CONTINUE ADDING CODE HERE!

% Gets number of empty spaces to left of a given board row column.
space_row_left(_, 0, 0).
space_row_left(BoardRow, Column, Space) :-
  Left is Column - 1,
  nth0(Left, BoardRow, Value),
  Value =@= ' ' ->
    space_row_left(BoardRow, Left, S), Space is 1 + S;
    Space is 0.

% Gets number of empty spaces to right of a given board row column.
space_row_right(_, 0, 0).
space_row_right(BoardRow, Column, Space) :-
  Right is Column + 1,
  nth0(Right, BoardRow, Value),
  Value =@= ' ' ->
    space_row_right(BoardRow, Right, S), Space is 1 + S;
    Space is 0.

% Gets number of empty spaces below a given board cell.
space_down(Board, Column, Row, Space) :-
  % Slice is a list of values in Column.
  column(Column, Board, Slice),
  % format('space_down: Slice = ~w~n', [Slice]),
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
space_up(Board, Column, Row, Space) :-
  % Slice is a list of values in Column.
  column(Column, Board, Slice),
  % format('space_up: Slice = ~w~n', [Slice]),
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
