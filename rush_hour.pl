% This finds solutions to Rush Hour puzzles.
% The board is a 6x6 grid.
% The cars have a color, start position, and end position.
% Each position is represented by an (X, Y) structure.
% X values go left to right from 0 to 5.
% Y values go top to bottom from 0 to 5.
% Each car is represented by a car(Color, Start, End) structure.
% Each color is a single letter code so the board can be printed nicely.
% The official color codes are as follows
% where the number is the car length:
% - x for red 2
% - a for light green 2
% - b for orange 2
% - c for blue 2
% - d for pink 2
% - e for purple 2
% - f for green 2
% - g for gray 2
% - h for tan 2
% - i for yellow 2
% - j for brown 2
% - k for olive 2
% - o for yellow 3
% - p for purple 3
% - q for blue 3
% - r for green 3

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

printRow_([H]) :- format('~w|~n', [H]), !.
printRow_([H|T]) :-
  write(H),
  write(' '),
  printRow_(T).
printRow(L) :-
  write('|'),
  printRow_(L).

printBoard(Board) :-
  length(Board, Size),
  Count is Size * 2 - 1,
  repeat('-', Count, Dashes),
  atomics_to_string(['+', Dashes, '+'], Border),
  writeln(Border),
  maplist(printRow, Board),
  writeln(Border).

% print(board(Size, Size, Exit, Cars)) :-

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

:- initialization
  /*
  % This is the set for puzzle #1.
  Cars = [
    car(a, (0, 0), (1, 0)), % light green 2
    car(p, (0, 1), (0, 3)), % purple 3
    car(x, (1, 2), (2, 2)), % red 2
    car(b, (0, 4), (0, 5)), % orange 2
    car(q, (3, 1), (3, 3)), % blue 3
    car(c, (4, 4), (5, 4)), % blue 2
    car(r, (2, 5), (4, 5)), % green 3
    car(o, (5, 0), (5, 2))  % yellow 3
  ],
  ExitRow = 2,
  Size = 6,
  Board = board(Size, Size, Exit, Cars),
  */

  Board = [
    ['A', ' ', ' ', ' ', ' ', ' '],
    [' ', 'B', ' ', ' ', ' ', ' '],
    [' ', ' ', 'C', ' ', ' ', ' '],
    [' ', ' ', ' ', 'D', ' ', ' '],
    [' ', ' ', ' ', ' ', 'E', ' '],
    [' ', ' ', ' ', ' ', ' ', 'F']
  ],
  printBoard(Board),
  halt.
