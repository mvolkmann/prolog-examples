// This is using default Prettier settings.
const SIZE = 6; // # of rows and columns on board
const SPACE = " ";

// This object holds static information about all possible cars.
const catalog = {
  X: { color: "red", length: 2 },
  A: { color: "mint", length: 2 },
  B: { color: "orange", length: 2 },
  C: { color: "cyan", length: 2 },
  D: { color: "pink", length: 2 },
  E: { color: "purple", length: 2 },
  F: { color: "green", length: 2 },
  G: { color: "gray", length: 2 },
  H: { color: "tan", length: 2 },
  I: { color: "lemon", length: 2 },
  J: { color: "brown", length: 2 },
  K: { color: "olive", length: 2 },
  O: { color: "yellow", length: 3 },
  P: { color: "mauve", length: 3 },
  Q: { color: "blue", length: 3 },
  R: { color: "teal", length: 3 },
  X: { color: "red", length: 2 },
};

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
    X: { row: 2, currentColumn: 1 },
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
    X: { row: 2, currentColumn: 1 },
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
    X: { row: 2, currentColumn: 3 },
  },
};

// This holds objects with cars and board properties
// that still need to be evaluated.
const pending = [];

// This holds position ids that have already been evaluated.
const visited = new Set();

// This updates the pending array.
// TODO: Change this to generate all possible moves,
// TODO: not just those with a distance of one.
function addMoves(letter, currentState) {
  const { board, cars } = currentState;

  const car = cars[letter];
  if (isHorizontal(car)) {
    const { row } = car;
    const startColumn = car.currentColumn;
    const endColumn = startColumn + catalog[letter].length - 1;
    const occupiedRow = board[row];

    const canMoveLeft =
      startColumn > 0 && occupiedRow[startColumn - 1] === SPACE;
    if (canMoveLeft) {
      const newCars = copyCars(cars);
      newCars[letter] = { row, currentColumn: startColumn - 1 };

      const newBoard = copyBoard(board);
      const newOccupiedRow = newBoard[row];
      newOccupiedRow[startColumn - 1] = letter;
      newOccupiedRow[endColumn] = SPACE;

      addPending(newBoard, newCars, `${letter} left`, currentState);
    }

    const canMoveRight =
      endColumn < SIZE - 1 && occupiedRow[endColumn + 1] === SPACE;
    if (canMoveRight) {
      const newCars = copyCars(cars);
      newCars[letter] = { row, currentColumn: startColumn + 1 };

      const newBoard = copyBoard(board);
      const newOccupiedRow = newBoard[row];
      newOccupiedRow[endColumn + 1] = letter;
      newOccupiedRow[startColumn] = SPACE;

      addPending(newBoard, newCars, `${letter} right`, currentState);
    }
  } else {
    // car is vertical
    const { column } = car;
    const startRow = car.currentRow;
    const endRow = startRow + catalog[letter].length - 1;

    const canMoveUp = startRow > 0 && board[startRow - 1][column] === SPACE;
    if (canMoveUp) {
      const newCars = copyCars(cars);
      newCars[letter] = { column, currentRow: startRow - 1 };

      const newBoard = copyBoard(board);
      newBoard[startRow - 1][column] = letter;
      newBoard[endRow][column] = SPACE;

      addPending(newBoard, newCars, `${letter} up`, currentState);
    }

    const canMoveDown =
      endRow < SIZE - 1 && board[endRow + 1][column] === SPACE;
    if (canMoveDown) {
      const newCars = copyCars(cars);
      newCars[letter] = { column, currentRow: startRow + 1 };

      const newBoard = copyBoard(board);
      newBoard[endRow + 1][column] = letter;
      newBoard[startRow][column] = SPACE;

      addPending(newBoard, newCars, `${letter} down`, currentState);
    }
  }
}

function addPending(board, cars, move, currentState) {
  const newState = { previousState: currentState, board, cars, move };
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

  const occupiedRows = [];

  // Create an empty board.
  for (let row = 0; row < SIZE; row++) {
    const occupiedColumns = Array(SIZE).fill(SPACE);
    occupiedRows.push(occupiedColumns);
  }

  // Add cars to the board.
  for (const letter of Object.keys(cars)) {
    const car = cars[letter];
    if (isHorizontal(car)) {
      const start = car.currentColumn;
      const end = start + catalog[letter].length;
      const occupiedRow = occupiedRows[car.row];
      for (let column = start; column < end; column++) {
        const existing = occupiedRow[column];
        if (existing !== SPACE) {
          console.error(`Car ${letter} overlaps car {existing}!`);
          process.exit(2);
        }
        occupiedRow[column] = letter;
      }
    } else {
      // car is vertical
      const { column } = car;
      const start = car.currentRow;
      const end = start + catalog[letter].length;
      for (let row = start; row < end; row++) {
        const occupiedRow = occupiedRows[row];
        occupiedRow[column] = letter;
      }
    }
  }

  return occupiedRows;
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

  // Get the row of exit which is always row 2.
  const row = board[2];

  // Check for cars blocking the exit.
  for (let column = startColumn; column < SIZE; column++) {
    if (row[column] !== SPACE) return false;
  }
  return true;
}

const isHorizontal = (car) => car.row !== undefined;

function printBoard(board, move) {
  if (move) console.log(move);
  for (const row of board) {
    console.log(row.join(SPACE));
  }
  console.log(); // blank line
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

function solve(cars) {
  const board = getBoard(cars);
  console.log("Starting board:");
  printBoard(board);
  addPending(board, cars);

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

solve(puzzles.p30);
