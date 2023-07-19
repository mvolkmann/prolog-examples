// To build this, enter "% swipl-ld -goal true -o calc calc.c calc.pl".
// The "-goal true" option prevents starting a top level.
// To run this, enter "./calc '{arithmetic-expression}'".
// For example, "./calc 'pi * 2'".
#include <stdio.h>
#include <string.h>
#include <SWI-Prolog.h>

#define MAXLINE 1024

void getGrandchildren() {
  // Find all the grandchildren of richard.
  module_t module = NULL;
  int flags = PL_Q_NORMAL;
  term_t arguments = PL_new_term_refs(2);
  predicate_t predicate = PL_predicate("grandchildren", 2, "user");
  PL_put_atom_chars(arguments, "richard");
  term_t variable = ???;
  PL_put_variable(arguments, variable);
  qid_t queryId = PL_open_query(module, flags, predicate, arguments);
  while (true) {
    int found = PL_next_solution(queryId);
    if (!found) break;
    // Print value of variable X.
    atom_t *name;
    // int status = PL_get_atom(variable, name)
    int status = PL_get_chars(variable, name, CVT_STRING);
    fprintf("grandchild = %s\n", name);
  }
  PL_cut_query(queryId); // discards the query
}

int main(int argc, char **argv) {
  if (argc <= 1) {
    fprintf(stderr, "An arithmetic expression must be provided.\n");
    return 2;
  }

  getGrandchildren();

  // Get the arithmetic expression to evaluate
  // from the first command-line argument.
  char expression[MAXLINE];
  char *address = expression;
  strcpy(address, argv[1]);

  // Build Prolog argument vector. 
  char *argumentVector[2];
  char *program = argv[0];
  argumentVector[0] = program;
  argumentVector[1] = NULL;

  // Attempt to initialize Prolog and halt if it fails.
  if (!PL_initialise(1, argumentVector)) PL_halt(1);

  // Lookup the calc/1 user predicate defined in calc.pl.
  predicate_t predicate = PL_predicate("calc", 1, "user");

  // Create a number of term references
  // needed by the calc predicate, 1 in this case.
  term_t arguments = PL_new_term_refs(1);

  // Add an atom to the argument list
  // that is created from the string "expression".
  PL_put_atom_chars(arguments, expression);

  // Call the predicate.
  // This is a shorthand for the combination of calls
  // PL_open_query(), PL_next_solution(), and PL_cut_query().
  // It generates a single solution. 
  module_t module = NULL;
  int flags = PL_Q_NORMAL;
  int solutionFound = PL_call_predicate(module, flags, predicate, arguments);

  // Halt the Prolog engine.
  PL_halt(solutionFound ? 0 : 1);

  return solutionFound;
}
