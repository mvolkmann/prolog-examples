const size = 6; // # of rows and columns on board

const cars = {
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

// Horizontal cars have a row property and
// vertical cars have a column property.
// The "current" properties give the starting position of the car.
// Row range from 0 (left) to 5 (right).
// Column values range from 0 (top) to 5 (bottom).
// The X car is always horizontal on row 2
// because the exit is on the right side of row 2.
const board = {
  A: { row: 0, currentColumn: 0 },
  B: { column: 0, currentRow: 4 },
  C: { row: 4, currentColumn: 4 },
  O: { column: 5, currentRow: 0 },
  P: { column: 0, currentRow: 1 },
  Q: { column: 3, currentRow: 1 },
  R: { row: 5, currentColumn: 2 },
  X: { row: 2, currentColumn: 1 }
}

function getCarEndPosition(board, carLetter) {
  const car = board[carLetter];
  const start = isHorizontal(car) ? car.currentColumn : car.currentRow;
  const { length } = cars[carLetter];
  return start + length - 1;
}

const isHorizontal = car => car.row !== undefined;

function printBoard(board) {
  const occupiedRows = [];
  for (let row = 0; row < size; row++) {
    const occupiedColumns = Array(size).fill(' ');
    occupiedRows.push(occupiedColumns);
  }

  for (const carLetter of Object.keys(board)) {
    const car = board[carLetter];
    if (isHorizontal(car)) {
      const start = car.currentColumn;
      const end = start + cars[carLetter].length;
      const occupiedRow = occupiedRows[car.row];
      for (let column = start; column < end; column++) {
        occupiedRow[column] = carLetter;
      }
    } else { // car is vertical
      const { column } = car;
      const start = car.currentRow;
      const end = start + cars[carLetter].length;
      for (let row = start; row < end; row++) {
        const occupiedRow = occupiedRows[row];
        occupiedRow[column] = carLetter;
      }
    }
  }

  for (let occupiedRow of occupiedRows) {
    console.log(occupiedRow.join(' '));
  }
}

printBoard(board);
