// TODO: Try to generalized this code toward handling any puzzle game.
// This is using default Prettier settings.
const EXIT_ROW = 2;
const SIZE = 6; // # of rows and columns on board
const SPACE = " ";

// TODO: This isn't currently used, but could be in a web app.
/*
const carColor = {
  X: "red",
  A: "mint",
  B: "orange",
  C: "cyan",
  D: "pink",
  E: "purple",
  F: "green",
  G: "gray",
  H: "tan",
  I: "lemon",
  J: "brown",
  K: "olive",
  O: "yellow",
  P: "mauve",
  Q: "blue",
  R: "teal",
  X: "red",
};
*/

const carLength = (letter) => ("OPQR".includes(letter) ? 3 : 2);

// This object holds information about the cars in a given puzzle.
// Horizontal cars have a row property and
// vertical cars have a column property.
// The "current" properties give the starting position of the car.
// Row range from 0 (left) to 5 (right).
// Column values range from 0 (top) to 5 (bottom).
// The X car is always horizontal on row 2
// because the exit is on the right side of row 2.

const puzzles = {
  p1: {
    A: { row: 0, currentColumn: 0 },
    B: { column: 0, currentRow: 4 },
    C: { row: 4, currentColumn: 4 },
    O: { column: 5, currentRow: 0 },
    P: { column: 0, currentRow: 1 },
    Q: { column: 3, currentRow: 1 },
    R: { row: 5, currentColumn: 2 },
    X: { row: EXIT_ROW, currentColumn: 1 },
  },
  p30: {
    A: { column: 2, currentRow: 0 },
    B: { column: 3, currentRow: 1 },
    C: { row: 3, currentColumn: 0 },
    D: { row: 3, currentColumn: 2 },
    E: { row: 5, currentColumn: 0 },
    F: { row: 5, currentColumn: 2 },
    O: { column: 0, currentRow: 0 },
    P: { row: 0, currentColumn: 3 },
    Q: { column: 5, currentRow: 3 },
    X: { row: EXIT_ROW, currentColumn: 1 },
  },
  p40: {
    A: { row: 0, currentColumn: 1 },
    B: { column: 4, currentRow: 0 },
    C: { column: 1, currentRow: 1 },
    D: { column: 2, currentRow: 1 },
    E: { column: 3, currentRow: 3 },
    F: { column: 2, currentRow: 4 },
    G: { row: 4, currentColumn: 4 },
    H: { row: 5, currentColumn: 0 },
    I: { row: 5, currentColumn: 3 },
    O: { column: 0, currentRow: 0 },
    P: { column: 5, currentRow: 1 },
    Q: { row: 3, currentColumn: 0 },
    X: { row: EXIT_ROW, currentColumn: 3 },
  },
};

// This holds objects with cars and board properties
// that still need to be evaluated.
const pending = [];

// This holds position ids that have already been evaluated.
const visited = new Set();

function addHorizontalMoves({
  state,
  letter,
  row,
  startColumn,
  endColumn,
  delta,
}) {
  const length = carLength(letter);
  const { currentColumn } = state.cars[letter];

  let column = startColumn;
  while (true) {
    // Make a copy of the cars objects where the car being moved is updated.
    const newCars = copyCars(state.cars);
    newCars[letter] = { row, currentColumn: column };

    // Make a copy of the board where the car being moved is updated.
    const newBoard = copyBoard(state.board);
    const newBoardRow = newBoard[row];
    // Remove car being moved.
    setRow(newBoardRow, startColumn, length, SPACE);
    // Add car being moved in new location.
    setRow(newBoardRow, column, length, letter);

    const distance = Math.abs(column - currentColumn);
    const direction = delta === -1 ? "right" : "left";
    const move = `${letter} ${direction} ${distance}`;
    console.log("addHorizontalMoves: move =", move);
    addPendingState(newBoard, newCars, move, state);

    if (column === endColumn) break;
    column += delta;
  }
}

function addVerticalMoves({ state, letter, column, startRow, endRow, delta }) {
  const length = carLength(letter);
  const { currentRow } = state.cars[letter];

  let row = startRow;
  while (true) {
    // Make a copy of the cars objects where the car being moved is updated.
    const newCars = copyCars(state.cars);
    newCars[letter] = { column, currentRow: row };

    // Make a copy of the board where the car being moved is updated.
    const newBoard = copyBoard(state.board);
    // Remove car being moved.
    setColumn(newBoard, column, startRow, length, SPACE);
    // Add car being moved in new location.
    setColumn(newBoard, column, row, length, letter);

    const distance = Math.abs(row - currentRow);
    const direction = delta === -1 ? "down" : "up";
    const move = `${letter} ${direction} ${distance}`;
    console.log("addVerticalMoves: move =", move);
    addPendingState(newBoard, newCars, move, state);

    if (row === endRow) break;
    row += delta;
  }
}

// This updates the pending array.
// TODO: Change this to generate all possible moves,
// TODO: not just those with a distance of one.
function addMoves(letter, state) {
  const { board, cars } = state;
  const length = carLength(letter);
  const car = cars[letter];

  if (isHorizontal(car)) {
    /*
    console.log(
      "generating horizontal moves for car",
      letter,
      "at column",
      car.currentColumn
    );
    */
    const { row } = car;
    const boardRow = board[row];

    // Generate moves to left from largest to smallest distance.
    const { currentColumn } = car;
    let startColumn = currentColumn;
    while (startColumn > 0 && boardRow[startColumn - 1] == SPACE) {
      startColumn--;
    }
    if (startColumn < currentColumn) {
      addHorizontalMoves({
        state,
        letter,
        row,
        startColumn,
        endColumn: car.currentColumn - 1,
        delta: 1,
      });
    }

    // Generate moves to right from largest to smallest distance.
    startColumn = car.currentColumn;
    const lastAllowed = SIZE - length;
    while (
      startColumn < lastAllowed &&
      boardRow[startColumn + length] == SPACE
    ) {
      startColumn++;
    }
    if (startColumn > currentColumn) {
      addHorizontalMoves({
        state,
        letter,
        row,
        startColumn,
        endColumn: car.currentColumn + 1,
        delta: -1,
      });
    }
  } else {
    // car is vertical
    /*
    console.log(
      "generating vertical moves for car",
      letter,
      "at row",
      car.currentRow
    );
    */
    const { column } = car;

    // Generate moves up from largest to smallest distance.
    const { currentRow } = car;
    let startRow = currentRow;
    while (startRow > 0 && board[startRow - 1][column] == SPACE) {
      startRow--;
    }
    if (startRow < currentRow) {
      addVerticalMoves({
        state,
        letter,
        column,
        startRow,
        endRow: car.currentRow - 1,
        delta: 1,
      });
    }

    // Generate moves down from largest to smallest distance.
    startRow = car.currentRow;
    const lastAllowed = SIZE - length;
    while (
      startRow < lastAllowed &&
      board[startRow + length][column] == SPACE
    ) {
      startRow++;
    }
    if (startRow > currentRow) {
      addVerticalMoves({
        state,
        letter,
        column,
        startRow,
        endRow: car.currentRow + 1,
        delta: -1,
      });
    }
  }
}

function addPendingState(board, cars, move, state) {
  const newState = { previousState: state, board, cars, move };
  pending.push(newState);
}

// This makes a deep copy of a board array.
function copyBoard(board) {
  const copy = [];
  for (const row of board) {
    copy.push([...row]);
  }
  return copy;
}

// This makes a deep copy of a cars object.
function copyCars(cars) {
  const copy = { ...cars };
  for (const letter of Object.keys(cars)) {
    const car = cars[letter];
    copy[letter] = { ...car };
  }
  return copy;
}

function getBoard(cars) {
  if (!cars.X) {
    console.error("Puzzle is missing car X!");
    process.exit(1);
  }

  const boardRows = [];

  // Create an empty board.
  for (let row = 0; row < SIZE; row++) {
    const occupiedColumns = Array(SIZE).fill(SPACE);
    boardRows.push(occupiedColumns);
  }

  // Add cars to the board.
  for (const letter of Object.keys(cars)) {
    const car = cars[letter];
    const length = carLength(letter);

    if (isHorizontal(car)) {
      const start = car.currentColumn;
      const end = start + length;
      const boardRow = boardRows[car.row];
      for (let column = start; column < end; column++) {
        const existing = boardRow[column];
        if (existing !== SPACE) {
          console.error(`Car ${letter} overlaps car {existing}!`);
          process.exit(2);
        }
        boardRow[column] = letter;
      }
    } else {
      // car is vertical
      const { column } = car;
      const start = car.currentRow;
      const end = start + length;
      for (let row = start; row < end; row++) {
        const boardRow = boardRows[row];
        boardRow[column] = letter;
      }
    }
  }

  return boardRows;
}

// This returns a string that uniquely describes a board position,
// but only for the current puzzle.
function getPositionId(cars) {
  return Object.values(cars)
    .map((car) =>
      car.currentColumn === undefined ? car.currentRow : car.currentColumn
    )
    .join("");
}

// The goal is reached when there are no cars blocking the X car from the exit.
function isGoalReached(board, cars) {
  // Get the column after the end of the X car.
  // This assumes the X car length is 2.
  const startColumn = cars.X.currentColumn + 2;

  const exitRow = board[EXIT_ROW];

  // Check for cars blocking the exit.
  for (let column = startColumn; column < SIZE; column++) {
    if (exitRow[column] !== SPACE) return false;
  }
  return true;
}

const isHorizontal = (car) => car.row !== undefined;

function printBoard(board) {
  console.log("+-----------+");
  board.forEach((row, index) => {
    let s = "|" + row.join(SPACE);
    if (index !== EXIT_ROW) s += "|";
    console.log(s);
  });
  console.log("+-----------+");
}

function printMoves(lastState) {
  // Get the moves.
  const moves = [];
  let state = lastState;
  while (state) {
    const { move } = state;
    const previousMove = moves[0];
    if (previousMove?.startsWith(move)) {
      const count = parseInt(previousMove.substring(move.length + 1));
      moves[0] = move + " " + (count + 1);
    } else if (move) {
      moves.unshift(move + " 1");
    }
    state = state.previousState;
  }

  for (const move of moves) {
    console.log(move);
  }
}

function setColumn(board, column, startRow, length, letter) {
  for (let r = startRow; r < startRow + length; r++) {
    board[r][column] = letter;
  }
}

function setRow(boardRow, startColumn, length, letter) {
  for (let c = startColumn; c < startColumn + length; c++) {
    boardRow[c] = letter;
  }
}

function solve(cars) {
  const board = getBoard(cars);
  console.log("Starting board:");
  printBoard(board);
  console.log(); // blank line

  addPendingState(board, cars);

  let lastState;

  while (pending.length > 0) {
    // TODO: Maybe use a heuristic to choose which pending state to try next.
    // TODO: For example, the one with the fewest cars blocking the exit.
    const pendingState = pending.shift();

    const { board, cars } = pendingState;

    if (isGoalReached(board, cars)) {
      lastState = pendingState;
      break;
    }

    const id = getPositionId(cars);
    if (!visited.has(id)) {
      visited.add(id);
      for (const letter of Object.keys(cars)) {
        addMoves(letter, pendingState);
      }
    }
  }

  if (lastState) {
    console.log("Solution found!\n");
    printMoves(lastState);
    console.log("\nFinal board:");
    printBoard(lastState.board);
  } else {
    console.log("No solution was found. :-(");
  }
}

// ----------------------------------------------------------------------------

solve(puzzles.p1);
