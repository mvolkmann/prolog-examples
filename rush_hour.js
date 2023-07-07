const size = 6; // # of rows and c, board, carsolumns on board
const space = " ";

// This object holds static information about all the possible cars.
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
const cars = {
  A: { row: 0, currentColumn: 0 },
  B: { column: 0, currentRow: 4 },
  C: { row: 4, currentColumn: 4 },
  O: { column: 5, currentRow: 0 },
  P: { column: 0, currentRow: 1 },
  Q: { column: 3, currentRow: 1 },
  R: { row: 5, currentColumn: 2 },
  X: { row: 2, currentColumn: 1 },
};

// This holds objects with cars and board properties
// that still need to be evaluated.
const pending = [];

// This holds position ids that have already been evaluated.
const visited = new Set();

// This updates the pending array.
function addMoves(carLetter, currentState) {
  const { board, cars } = currentState;

  const car = cars[carLetter];
  if (isHorizontal(car)) {
    const { row } = car;
    const startColumn = car.currentColumn;
    const endColumn = startColumn + catalog[carLetter].length - 1;
    const occupiedRow = board[row];

    const canMoveLeft =
      startColumn > 0 && occupiedRow[startColumn - 1] === space;
    if (canMoveLeft) {
      const newCars = copyCars(cars);
      newCars[carLetter] = { row, currentColumn: startColumn - 1 };

      const newBoard = copyBoard(board);
      const newOccupiedRow = newBoard[row];
      newOccupiedRow[startColumn - 1] = carLetter;
      newOccupiedRow[endColumn] = space;

      addPending(newBoard, newCars, `${carLetter} left`, currentState);
    }

    const canMoveRight =
      endColumn < size - 1 && occupiedRow[endColumn + 1] === space;
    if (canMoveRight) {
      const newCars = copyCars(cars);
      newCars[carLetter] = { row, currentColumn: startColumn + 1 };

      const newBoard = copyBoard(board);
      const newOccupiedRow = newBoard[row];
      newOccupiedRow[endColumn + 1] = carLetter;
      newOccupiedRow[startColumn] = space;

      addPending(newBoard, newCars, `${carLetter} right`, currentState);
    }
  } else {
    // car is vertical
    const { column } = car;
    const startRow = car.currentRow;
    const endRow = startRow + catalog[carLetter].length - 1;

    const canMoveUp = startRow > 0 && board[startRow - 1][column] === space;
    if (canMoveUp) {
      const newCars = copyCars(cars);
      newCars[carLetter] = { column, currentRow: startRow - 1 };

      const newBoard = copyBoard(board);
      newBoard[startRow - 1][column] = carLetter;
      newBoard[endRow][column] = space;

      addPending(newBoard, newCars, `${carLetter} up`, currentState);
    }

    const canMoveDown =
      endRow < size - 1 && board[endRow + 1][column] === space;
    if (canMoveDown) {
      const newCars = copyCars(cars);
      newCars[carLetter] = { column, currentRow: startRow + 1 };

      const newBoard = copyBoard(board);
      newBoard[endRow + 1][column] = carLetter;
      newBoard[startRow][column] = space;

      addPending(newBoard, newCars, `${carLetter} down`, currentState);
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
  for (const carLetter of Object.keys(cars)) {
    const car = cars[carLetter];
    copy[carLetter] = { ...car };
  }
  return copy;
}

function getBoard() {
  const occupiedRows = [];

  // Create an empty board.
  for (let row = 0; row < size; row++) {
    // TODO: Maybe occupiedColumns should be a 6-char string instead of array.
    const occupiedColumns = Array(size).fill(space);
    occupiedRows.push(occupiedColumns);
  }

  // Add cars to the board.
  for (const carLetter of Object.keys(cars)) {
    const car = cars[carLetter];
    if (isHorizontal(car)) {
      const start = car.currentColumn;
      const end = start + catalog[carLetter].length;
      const occupiedRow = occupiedRows[car.row];
      for (let column = start; column < end; column++) {
        occupiedRow[column] = carLetter;
      }
    } else {
      // car is vertical
      const { column } = car;
      const start = car.currentRow;
      const end = start + catalog[carLetter].length;
      for (let row = start; row < end; row++) {
        const occupiedRow = occupiedRows[row];
        occupiedRow[column] = carLetter;
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
  for (let column = startColumn; column < size; column++) {
    if (row[column] !== space) return false;
  }
  return true;
}

const isHorizontal = (car) => car.row !== undefined;

function printBoard(board, move) {
  if (move) console.log(move);
  for (const row of board) {
    console.log(row.join(space));
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

function solve() {
  addPending(getBoard(), cars);

  let lastState;

  while (pending.length > 0) {
    const pendingState = pending.shift();
    const { board, cars } = pendingState;

    if (isGoalReached(board, cars)) {
      lastState = pendingState;
      break;
    }

    const id = getPositionId(cars);
    if (!visited.has(id)) {
      visited.add(id);
      for (const carLetter of Object.keys(cars)) {
        addMoves(carLetter, pendingState);
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

solve();
