// This solves Rush Hour puzzles.
// See https://en.wikipedia.org/wiki/Rush_Hour_(puzzle).
// It uses a search strategy that is similar to A*
// (https://en.wikipedia.org/wiki/A*_search_algorithm), but doesn't
// use a heuristic function to select the next node to evaluate.
//
// This code is formatted using default Prettier settings.

const EXIT_ROW = 2;
const SIZE = 6; // # of rows and columns on board
const BORDER = "+" + "-".repeat(SIZE * 2 - 1) + "+";
const SPACE = " ";

// This object holds information about the cars in a given puzzle.
// The "fixed" properties give the row for horizontal cars
// and the column for vertical cars.
// The "variable" properties give the starting column for horizontal cars
// and the starting row for vertical cars.
// Rows range from 0 (left) to 5 (right).
// Columns range from 0 (top) to 5 (bottom).
// The X car is always horizontal on row 2
// because the exit is on the right side of row 2.
const PUZZLES = {
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
    X: { fixed: EXIT_ROW, variable: 1, horizontal: true },
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
    X: { fixed: EXIT_ROW, variable: 3, horizontal: true },
  }
};

// This holds boolean values that indicate whether each car is horizontal.
let isHorizontal = [];

// This holds the fixed position of each car.
let fixedPositions = [];

// This holds all the car letters used in the current puzzle.
// It is set in the solve function.
let letters = [];

// This holds objects with the properties
// "move", "variablePositions", "board", and "previousState".
// These objects describe states that still need to be evaluated
// and will not necessarily be part of the solutions.
// This is key to implementing a breadth-first search.
const pendingStates = [];

// This holds state ids that have already been evaluated.
// It is used to avoid evaluating a board state multiple times.
const visitedIds = new Set();

// These functions translate between
// car letters and their indexes in arrays.
const aAscii = 'A'.charCodeAt(0);
const letterToIndex = letter => letter.charCodeAt(0) - aAscii;
const indexToLetter = index => String.fromCharCode(index + aAscii);

// This gets the length of a car from its index.
// Cars O, P, Q, and R have a length of 3.
// All other cars have a length of 2.
const oIndex = 'O'.charCodeAt(0) - aAscii;
const rIndex = 'R'.charCodeAt(0) - aAscii;
const xIndex = letterToIndex('X'); // used in isGoalReached function
const carLength = index => oIndex <= index && index <= rIndex ? 3 : 2;

function addHorizontalMoves({
  state,
  letter,
  row,
  startColumn,
  endColumn,
  delta,
}) {
  const { board, variablePositions } = state;
  const index = letterToIndex(letter);
  const currentColumn = variablePositions[index];
  const length = carLength(index);

  let column = startColumn;
  while (true) {
    // Make a copy of variablePositions where the car being moved is updated.
    const newPositions = [...variablePositions];
    newPositions[index] = column;

    // Make a copy of the board where the car being moved is updated.
    const newBoard = copyBoard(board);
    const newBoardRow = newBoard[row];
    // Remove car being moved.
    setRow(newBoardRow, SPACE, currentColumn, length);
    // Add car being moved in new location.
    setRow(newBoardRow, letter, column, length);
    // printBoard(newBoard);

    const direction = delta === -1 ? "right" : "left";
    const distance = Math.abs(column - currentColumn);
    const move = `${letter} ${direction} ${distance}`;
    addPendingState(newBoard, newPositions, move, state);

    if (column === endColumn) break;
    column += delta;
  }
}

function addVerticalMoves({ state, letter, column, startRow, endRow, delta }) {
  const { board, variablePositions } = state;
  const index = letterToIndex(letter);
  const currentRow = variablePositions[index];
  const length = carLength(index);

  let row = startRow;
  while (true) {
    // Make a copy of variablePositions where the car being moved is updated.
    const newPositions = [...variablePositions];
    newPositions[index] = row;

    // Make a copy of the board where the car being moved is updated.
    const newBoard = copyBoard(board);
    // Remove car being moved.
    setColumn(newBoard, SPACE, column, currentRow, length);
    // Add car being moved in new location.
    setColumn(newBoard, letter, column, row, length);

    const direction = delta === -1 ? "down" : "up";
    const distance = Math.abs(row - currentRow);
    const move = `${letter} ${direction} ${distance}`;
    addPendingState(newBoard, newPositions, move, state);

    if (row === endRow) break;
    row += delta;
  }
}

// This adds states to be evaluated to the pendingStates array.
function addMoves(letter, state) {
  const { board, variablePositions } = state;
  const index = letterToIndex(letter);
  const length = carLength(index);

  if (isHorizontal[index]) {
    const row = fixedPositions[index];
    const boardRow = board[row];
    const currentColumn = variablePositions[index];

    // Find the largest distance this car can be moved left.
    let startColumn = currentColumn;
    while (startColumn > 0 && boardRow[startColumn - 1] == SPACE) {
      startColumn--;
    }

    if (startColumn < currentColumn) {
      // Generate moves to left from largest to smallest distance.
      addHorizontalMoves({
        state,
        letter,
        row,
        startColumn,
        endColumn: currentColumn - 1,
        delta: 1,
      });
    }

    // Find the largest distance this car can be moved right.
    startColumn = currentColumn;
    const lastAllowed = SIZE - length;
    while (
      startColumn < lastAllowed &&
      boardRow[startColumn + length] == SPACE
    ) {
      startColumn++;
    }

    if (startColumn > currentColumn) {
      // Generate moves to right from largest to smallest distance.
      addHorizontalMoves({
        state,
        letter,
        row,
        startColumn,
        endColumn: currentColumn + 1,
        delta: -1,
      });
    }
  } else {
    // The car is vertical.
    const column = fixedPositions[index];
    const currentRow = variablePositions[index];

    // Find the largest distance this car can be moved up.
    let startRow = currentRow;
    while (startRow > 0 && board[startRow - 1][column] == SPACE) {
      startRow--;
    }

    if (startRow < currentRow) {
      // Generate moves up from largest to smallest distance.
      addVerticalMoves({
        state,
        letter,
        column,
        startRow,
        endRow: currentRow - 1,
        delta: 1,
      });
    }

    // Find the largest distance this car can be moved down.
    startRow = currentRow;
    const lastAllowed = SIZE - length;
    while (
      startRow < lastAllowed &&
      board[startRow + length][column] == SPACE
    ) {
      startRow++;
    }

    if (startRow > currentRow) {
      // Generate moves down from largest to smallest distance.
      addVerticalMoves({
        state,
        letter,
        column,
        startRow,
        endRow: currentRow + 1,
        delta: -1,
      });
    }
  }
}

function addPendingState(board, variablePositions, move, previousState) {
  const newState = { board, variablePositions, move, previousState };
  pendingStates.push(newState);
}

// This makes a deep copy of a board array.
function copyBoard(board) {
  const copy = [];
  for (const row of board) {
    copy.push([...row]);
  }
  return copy;
}

// This creates a 2D array of car letters for a given puzzle.
function getBoard(variablePositions) {
  const boardRows = [];

  // Create an empty board.
  for (let row = 0; row < SIZE; row++) {
    const boardRow = Array(SIZE).fill(SPACE);
    boardRows.push(boardRow);
  }

  // Add cars to the board.
  for (const letter of letters) {
    const index = letterToIndex(letter);
    const length = carLength(index);

    if (isHorizontal[index]) {
      const row = fixedPositions[index];
      const startColumn = variablePositions[index];
      const endColumn = startColumn + length;
      const boardRow = boardRows[row];
      for (let column = startColumn; column < endColumn; column++) {
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
      const column = fixedPositions[index];
      const startRow = variablePositions[index];
      const endRow = startRow + length;
      for (let row = startRow; row < endRow; row++) {
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
}

// This returns a string that uniquely describes a board state,
// but only for the current puzzle.
const getStateId = positions => positions.filter(p => p !== null).join('');

// The goal is reached when there are no cars blocking the X car from the exit.
function isGoalReached(board, variablePositions) {
  // Get the column after the end of the X car.
  // This assumes the X car length is 2.
  const startColumn = variablePositions[xIndex] + 2;

  const exitRow = board[EXIT_ROW];

  // Check for cars blocking the exit.
  for (let column = startColumn; column < SIZE; column++) {
    if (exitRow[column] !== SPACE) return false;
  }
  return true;
}

function printBoard(board) {
  console.log(BORDER);
  // We need to use forEach instead of a "for of" loop
  // so we have the index at each iteration.
  board.forEach((row, index) => {
    let s = "|" + row.join(SPACE);
    if (index !== EXIT_ROW) s += "|";
    console.log(s);
  });
  console.log(BORDER);
}

function printMoves(lastState) {
  // Get the solution moves by walking backwards from the final state.
  const moves = [];
  let state = lastState;
  // This first state doesn't have a "move" property.
  while (state.move) {
    moves.push(state.move);
    state = state.previousState;
  }

  // The moves are in reverse order, so print them from the last to the first.
  for (let i = moves.length - 1; i >= 0; i--) {
    console.log(moves[i]);
  }
}

// This sets the board letter used in a range of rows for a given column.
function setColumn(board, letter, column, startRow, length) {
  for (let r = startRow; r < startRow + length; r++) {
    board[r][column] = letter;
  }
}

// This sets the board letter used in a range of columns for a given row.
function setRow(boardRow, letter, startColumn, length) {
  for (let c = startColumn; c < startColumn + length; c++) {
    boardRow[c] = letter;
  }
}

// This solves a given puzzle.
function solve(puzzle) {
  if (!puzzle) {
    console.error("Puzzle not found!");
    process.exit(1);
  }
  if (!puzzle.X) {
    console.error("Puzzle is missing car X!");
    process.exit(2);
  }

  letters = Object.keys(puzzle);

  // This holds the variable position of each car.
  let variablePositions = [];

  for (const letter of letters) {
    const car = puzzle[letter];
    const index = letterToIndex(letter);
    isHorizontal[index] = car.horizontal;
    fixedPositions[index] = car.fixed;
    variablePositions[index] = car.variable;
  }

  let board = getBoard(variablePositions);
  console.log("Starting board:");
  printBoard(board);
  console.log(); // blank line

  // The initial state has no move or previous state.
  addPendingState(board, variablePositions);

  // This is set when a solution is found.
  let lastState;

  // While there are more states to evaluate ...
  while (pendingStates.length > 0) {
    // Get the next state to evaluate.
    // We could use a heuristic to choose which pending state to try next.
    // For example, we could select the state
    // with the fewest cars blocking the exit.
    // But I suspect the time saved would be not be as much
    // as the time required to compute the heuristic.
    // The only kind of heuristic currently used is to
    // evaluate longer moves before shorter ones.
    const pendingState = pendingStates.shift();

    ({ board, variablePositions } = pendingState);

    if (isGoalReached(board, variablePositions)) {
      lastState = pendingState;
      break; // finished searching for a solution
    }

    // Ensure that we won't evaluate this same state again.
    const id = getStateId(variablePositions);
    if (!visitedIds.has(id)) {
      visitedIds.add(id);

      // Find all moves that can be made in the current state and
      // save them in pendingStates for possible evaluation later.
      for (const letter of letters) {
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

solve(PUZZLES.p40);
