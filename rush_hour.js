const size = 6; // # of rows and columns on board

// This object holds static information about all the possible cars.
const catalog = {
  X: { color: 'red', length: 2 },
  A: { color: 'mint', length: 2 },
  B: { color: 'orange', length: 2 },
  C: { color: 'cyan', length: 2 },
  D: { color: 'pink', length: 2 },
  E: { color: 'purple', length: 2 },
  F: { color: 'green', length: 2 },
  G: { color: 'gray', length: 2 },
  H: { color: 'tan', length: 2 },
  I: { color: 'lemon', length: 2 },
  J: { color: 'brown', length: 2 },
  K: { color: 'olive', length: 2 },
  O: { color: 'yellow', length: 3 },
  P: { color: 'mauve', length: 3 },
  Q: { color: 'blue', length: 3 },
  R: { color: 'teal', length: 3 },
  X: { color: 'red', length: 2 }
}

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
  X: { row: 2, currentColumn: 1 }
}

// TODO: Is this needed?
function getCarEndPosition(carLetter) {
  const car = cars[carLetter];
  const start = isHorizontal(car) ? car.currentColumn : car.currentRow;
  const { length } = catalog[carLetter];
  return start + length - 1;
}

function getBoard() {
  const occupiedRows = [];

  // Create an empty board.
  for (let row = 0; row < size; row++) {
    const occupiedColumns = Array(size).fill(' ');
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
    } else { // car is vertical
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
    .map(car => car.currentColumn === undefined ?
      car.currentRow :
      car.currentColumn
    )
    .join('');
}

function getValidMoves(carLetter) {

}

// The goal is reached when there are no cars blocking the X car from the exit.
function isGoalReached() {
  // Get the column after the end of the X car.
  // This assumes the X car length is 2.
  const startColumn = cars.X.currentColumn + 2;

  // Get the row of exit which is always row 2.
  const row = board[2];

  // Check for cars blocking the exit.
  for (let column = startColumn; column < size; column++) {
    if (row[column] !== ' ') return false;
  }
  return true;
}

const isHorizontal = car => car.row !== undefined;

function printBoard(board) {
  for (let occupiedRow of board) {
    console.log(occupiedRow.join(' '));
  }
}

// This is a two dimensional array that represents the current board.
const board = getBoard();
printBoard(board);
console.log('Goal reached?', isGoalReached());
console.log('position id =', getPositionId(cars));
