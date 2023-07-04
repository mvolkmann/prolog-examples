% This is based on code from Markus Triska in the SWI-Prolog manual.

:- use_module(library(clpfd)).

sudoku(Rows) :-
  % Verify that Rows is a list with 9 elements.
  length(Rows, 9),

  % Verify that all elements are lists
  % with the same length as Rows which is 9.
  maplist(same_length(Rows), Rows),

  % Create a flattened list of all the values (Vs), and verify
  % that all elements in Vs are a number in the range 1 to 9.
  append(Rows, Vs), Vs ins 1..9,

  % Verify that all element values in all rows
  % are unique within their row.
  maplist(all_distinct, Rows),

  % Create a list of lists that represent the columns.
  transpose(Rows, Columns),

  % Verify that all element values in all columns
  % are unique within their column.
  maplist(all_distinct, Columns),

  % Assign a variable name to each of the 9 rows.
  [R1, R2, R3, R4, R5, R6, R7, R8, R9] = Rows,

  % Verify that the element values in every 3x3 block
  % are unique within their block.
  blocks(R1, R2, R3),
  blocks(R4, R5, R6),
  blocks(R7, R8, R9).

% When a block is empty, its element values (which are none)
% can be considered unique.
blocks([], [], []).

% When a block is not empty, get its 9 values
% and verify that they are unique.
blocks([R1C1,R1C2,R1C3|T1], [R2C1,R2C2,R2C3|T2], [R3C1,R3C2,R3C3|T3]) :-
  all_distinct([R1C1, R1C2, R1C3, R2C1, R2C2, R2C3, R3C1, R3C2, R3C3]),
  blocks(T1, T2, T3).

% When there a no more rows, stop printing.
print_rows([]).

% When there are more rows, print the first row.
print_rows([H|T]) :- print_row(H), print_rows(T).

% When the last element of a row has been printed, print a newline.
print_row([]) :- nl.

% When there are more row elements,
% print the first one followed by a space.
print_row([H|T]) :- format("~w ", H), print_row(T).

% Each puzzle must contain at least 17 clues.

problem(1, % can solve
  [[_,_,_, _,_,_, _,_,_],
   [_,_,_, _,_,3, _,8,5],
   [_,_,1, _,2,_, _,_,_],

   [_,_,_, 5,_,7, _,_,_],
   [_,_,4, _,_,_, 1,_,_],
   [_,9,_, _,_,_, _,_,_],

   [5,_,_, _,_,_, _,7,3],
   [_,_,2, _,1,_, _,_,_],
   [_,_,_, _,4,_, _,_,9]]).

% See comments at https://www.youtube.com/watch?v=5KUdEZTu06o.
problem(2, % cannot solve
  [[5,_,9, _,_,_, _,_,_], % doesn't work
  % [[5,_,9, _,_,_, _,_,8], % works with this extra clue
   [_,7,_, _,_,6, _,_,1],
   [6,_,_, _,9,_, _,_,4],

   [_,_,_, _,_,9, _,5,_],
   [_,4,_, _,_,3, _,8,_],
   [_,5,8, _,7,_, _,2,_],

   [_,6,_, 2,_,_, _,4,_],
   [_,8,_, _,_,7, _,_,3],
   [_,_,_, _,_,_, _,_,_]]).

problem(3, % can solve
  [[_,2,_, 6,_,8, _,_,_],
   [5,8,_, _,_,9, 7,_,_],
   [_,_,_, _,4,_, _,_,_],

   [3,7,_, _,_,_, 5,_,_],
   [6,_,_, _,_,_, _,_,4],
   [_,_,8, _,_,_, _,1,3],

   [_,_,_, _,2,_, _,_,_],
   [_,_,9, 8,_,_, _,3,6],
   [_,_,_, 3,_,6, _,9,_]]).

:- problem(2, Rows), sudoku(Rows), print_rows(Rows).
