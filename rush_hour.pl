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

% Define facts about the length of each car.
car_length(a, 2).
car_length(b, 2).
car_length(c, 2).
car_length(d, 2).
car_length(e, 2).
car_length(f, 2).
car_length(g, 2).
car_length(h, 2).
car_length(i, 2).
car_length(j, 2).
car_length(k, 2).
car_length(o, 3).
car_length(p, 3).
car_length(q, 3).
car_length(r, 3).
car_length(x, 2).

% You don't need a rule to make a deep copy of a board nested list.
% Just use the copy_term(In, Out) predicate.

add_car(Puzzle, Board, Letter, NewBoard) :-
  Car = Puzzle.get(Letter),
  car_length(Letter, Length),

  % Dynamically create facts describing which cars are horizontal.
  H = Car.get(horizontal, false),
  (H == true -> assertz(horizontal(Letter)); true),

  % Dynamically create facts describing the fixed position of each car.
  Fixed = Car.fixed,
  assertz(fixed(Letter, Fixed)),

  % Dynamically create facts that map each car letter to an index
  % that is used in the Fixed list.
  dict_keys(Puzzle, Letters),
  nth0(Index, Letters, Letter),
  assertz(letter_index(Letter, Index)),

  Variable = Car.variable,
  (H == true ->
    set_row(Board, Letter, Fixed, Variable, Length, NewBoard);
    set_column(Board, Letter, Fixed, Variable, Length, NewBoard)
  ).

add_moves([Board, Variables], Letter) :-
  (horizontal(Letter) ->
    % For horizontal cars ...
    moves_left(Board, Variables, Letter),
    moves_right(Board, Variables, Letter);

    % For vertical cars ...
    moves_up(Board, Variables, Letter),
    moves_down(Board, Variables, Letter)
  ).

add_horizontal_moves(Variables, Board, Letter, Row, StartColumn, A, B, Delta) :-
  % Create a new board that represents the move.
  car_length(Letter, Length),
  set_row(Board, ' ', Row, StartColumn, Length, Board2),
  set_row(Board2, Letter, Row, A, Length, Board3),

  % Create a new list of variable values where
  % the value at the index for the given Letter is changed to A.
  letter_index(Letter, Index),
  replace(Variables, Index, A, NewVariables),

  add_pending_state([Board3, NewVariables]),

  nb_getval(log, Log),
  (Log == true ->
    atomics_to_string([Letter, ' moving horizontally to ', A], Move),
    writeln(Move);
    true
  ),

  % If there are more open spaces remaining to evaluate ...
  (A =\= B ->
    Next #= A + Delta,
    add_horizontal_moves(Variables, Board, Letter, Row, StartColumn, Next, B, Delta);
    true
  ).

% This adds a given state to the end of the pending list
% only if it has not already been visited.
add_pending_state(State) :-
  nb_getval(visitedIds, VisitedIds),
  state_key(State, StateKey),
  Visited = VisitedIds.get(StateKey, false),
  (Visited == false ->
    nb_getval(pendingStates, PendingStates),
    append(PendingStates, [State], NewPendingStates),
    nb_setval(pendingStates, NewPendingStates);
    true
  ).

add_vertical_moves(Variables, Board, Letter, Column, StartRow, A, B, Delta) :-
  % Create a new board that represents the move.
  car_length(Letter, Length),
  set_column(Board, ' ', Column, StartRow, Length, Board2),
  set_column(Board2, Letter, Column, A, Length, Board3),

  % Create a new list of variable values where
  % the value at the index for the given Letter is changed to A.
  letter_index(Letter, Index),
  replace(Variables, Index, A, NewVariables),

  add_pending_state([Board3, NewVariables]),

  nb_getval(log, Log),
  (Log == true ->
    atomics_to_string([Letter, ' moving vertically to ', A], Move),
    writeln(Move);
    true
  ),

  % If there are more open spaces remaining to evaluate ...
  (A =\= B ->
    Next #= A + Delta,
    add_vertical_moves(Variables, Board, Letter, Column, StartRow, Next, B, Delta);
    true
  ).

% Gets a border string used when printing a board.
border(B) :-
  size(Size),
  Count is Size * 2 - 1,
  repeat('-', Count, Dashes),
  atomics_to_string(['+', Dashes, '+'], B).

% Gets the variable property of the car
% with a given letter in a given puzzle.
car_variable(Puzzle, Letter, Variable) :-
  Car = Puzzle.get(Letter),
  Variable = Car.get(variable).

% This creates a list that represents an empty board row.
empty_board_row(Row) :-
  size(Size),
  length(Row, Size),
  maplist(=(' '), Row).

% This creates a list that represents an empty board row.
% It is needed for compatibility with being called using maplist.
empty_board_row(_, Row) :-
  empty_board_row(Row).

% This creates a nested list that represents an empty board.
empty_board(Board) :-
  size(Size),
  length(Rows, Size),
  maplist(empty_board_row, Rows, Board).

% This determines if there are any cars blocking the x car exit.
goal_reached(Board) :-
  nth0(2, Board, Row2),
  tail_after_last(x, Row2, T),
  maplist(=(' '), T).

moves(nil, []) :- !.
moves(State, L) :-
  moves(State.previousState, L2),
  Move = State.get(move, ''),
  append(L2, [Move], L).

moves_down(Board, Variables, Letter) :-
  fixed(Letter, Column),
  letter_index(Letter, Index),
  nth0(Index, Variables, StartRow),
  car_length(Letter, Length),
  EndRow #= StartRow + Length - 1,
  space_down(Board, Column, EndRow, Space),
  (Space #> 0 ->
    A #= StartRow + Space,
    B #= StartRow + 1,
    add_vertical_moves(Variables, Board, Letter, Column, StartRow, A, B, 1);
    true
  ).

moves_left(Board, Variables, Letter) :-
  fixed(Letter, Row),
  letter_index(Letter, Index),
  nth0(Index, Variables, StartColumn),
  space_left(Board, Row, StartColumn, Space),
  (Space #> 0 ->
    A #= StartColumn - Space,
    B #= StartColumn - 1,
    add_horizontal_moves(Variables, Board, Letter, Row, StartColumn, A, B, 1);
    true
  ).

moves_right(Board, Variables, Letter) :-
  fixed(Letter, Row),
  letter_index(Letter, Index),
  nth0(Index, Variables, StartColumn),
  car_length(Letter, Length),
  EndColumn #= StartColumn + Length - 1,
  space_right(Board, Row, EndColumn, Space),
  (Space #> 0 ->
    A #= StartColumn + Space,
    B #= StartColumn + 1,
    add_horizontal_moves(Variables, Board, Letter, Row, StartColumn, A, B, -1);
    true
  ).

moves_up(Board, Variables, Letter) :-
  fixed(Letter, Column),
  letter_index(Letter, Index),
  nth0(Index, Variables, StartRow),
  space_up(Board, Column, StartRow, Space),
  (Space #> 0 ->
    A #= StartRow - Space,
    B #= StartRow - 1,
    add_vertical_moves(Variables, Board, Letter, Column, StartRow, A, B, 1);
    true
  ).

print_board(Board) :- write_board(user_output, Board).

/*
print_moves(State) :-
  moves_string(State, S),
  write(S).
*/

print_state([Board, _]) :- print_board(Board).

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

search(_, [], _) :-
  writeln('No solution was found. :-('),
  halt.

search(Letters, [State|Rest], Iterations) :-
  % atomics_to_string([Letter, ' moves to row ', A], Move),
  (goal_reached(State) ->
    writeln('Solution found!'), halt;
    true
  ),

  state_key(State, StateKey),
  format('search: StateKey = ~w~n', [StateKey]),
  print_state(State),

  (StateKey == 's33400121' ->
    nb_setval(log, true);
    nb_setval(log, false)
  ),

  % We can assume this state has not yet been visited because
  % we only add states to the pending list that have not been visited.

  % Record this state as having been visited.
  nb_getval(visitedIds, VisitedIds),
  NewVisitedIds = VisitedIds.put(StateKey, true),
  nb_setval(visitedIds, NewVisitedIds),

  % Remove this state from the list of pending states.
  nb_setval(pendingStates, Rest),

  % Evaluate all the valid moves from the current state.
  maplist(add_moves(State), Letters),

  % Search the next pending state.
  %(Iterations < 10 ->
    % The calls to add_move likely updated pending states list,
    % so get the new value.
    nb_getval(pendingStates, PendingStates),

    Next is Iterations + 1,
    search(Letters, PendingStates, Next). %;
    %true
    %).

solve(Puzzle) :-
  % Verify that the puzzle contains an X car.
  (_ = Puzzle.get(x) -> true; writeln('Puzzle is missing car X!')),

  % Place the cars on the board in their initial positions.
  empty_board(Board),
  dict_keys(Puzzle, Letters),
  foldl(add_car(Puzzle), Letters, Board, NewBoard),

  % Print the starting board so we can verify that the puzzle is correct.
  writeln('Starting board:'),
  print_board(NewBoard),

  % Set Variables to a list of the initial variable positions of each car.
  maplist(car_variable(Puzzle), Letters, Variables),

  % Each state is a list contain a board representation
  % and a list of variable positions of each car.
  InitialState = [NewBoard, Variables],

  % Create a list to hold the pending states to evaluate.
  PendingStates = [InitialState],
  nb_setval(pendingStates, PendingStates),

  % Create an empty dict to hold the ids of visited states.
  nb_setval(visitedIds, ids{}),

  % Begin the search for a solution.
  search(Letters, PendingStates, 1).

solve_p1 :-
  puzzles(Puzzles),
  solve(Puzzles.p1).

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
  space_row_left(Slice, Row, Space).

% This gets a unique id for a given state.
state_key(State, StateKey) :-
  [_, Variables] = State,
  atomics_to_string(['s' | Variables], StateId),
  atom_string(StateKey, StateId).

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

:- initialization((
  dynamic(horizontal/1),
  dynamic(fixed/2),
  dynamic(letter_index/2).

  /* You can run "swipl rush_hour.plt" instead of using this code.
  puzzles(Puzzles), solve(Puzzles.p1),
  halt
  */
)).
