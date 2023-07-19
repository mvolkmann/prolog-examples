// To build this, enter "% swipl-ld -goal true -o calc calc.c calc.pl".
// The "-goal true" option prevents starting a top level.
// To run this, enter "./calc '{arithmetic-expression}'".
// For example, "./calc 'pi * 2'".
#include <stdio.h>
#include <string.h>
#include <SWI-Prolog.h>

#define MAXLINE 1024

int main(int argc, char **argv) {
  if (argc <= 1) {
    fprintf(stderr, "An arithmetic expression must be provided.\n");
    return 2;
  }

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
  module_t module = NULL;
  int flags = PL_Q_NORMAL;
  // This is a shorthand for the combination of calls
  // PL_open_query(), PL_next_solution(), and PL_cut_query().
  // It generates a single solution. 
  int solutionFound = PL_call_predicate(module, flags, predicate, arguments);

  // Halt the Prolog engine.
  PL_halt(solutionFound ? 0 : 1);

  return solutionFound;
}
