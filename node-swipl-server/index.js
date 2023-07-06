const swipl = require('swipl');

function loadFile(directory, fileName) {
  swipl.call(`working_directory(_, '${directory}')`);
  swipl.call(`consult(${fileName})`)
}

function printSolution(goal, solution) {
  console.log('Solution for', goal);
  if (solution) {
    console.log(' ', solution.X);
  } else {
    console.error('No solution returned');
  }
}

function printSolutions(goal, query) {
  console.log('Solutions for', goal);
  while (query && (solution = query.next())) {
    console.log(' ', solution.X);
  }
  query.close();
}

// This only gets the first solution.
let goal = 'member(X, [1,2,3,4])';
printSolution(goal, swipl.call('member(X, [1,2,3,4])'));

// This gets all solutions.
// Only one query can be open at a time.
let query = new swipl.Query(goal);
printSolutions(goal, query);

// This loads a Prolog source file and runs a query against it.
loadFile('..', 'exercise1_3');
goal = 'grandfather_of(richard, X)';
query = new swipl.Query(goal);
printSolutions(goal, query);
