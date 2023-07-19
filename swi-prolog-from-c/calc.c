// To build this, enter "% swipl-ld -goal true -o calc calc.c calc.pl".
// The "-goal true" option prevents starting a top level.
// To run this, enter "./calc '{arithmetic-expression}'".
// For example, "./calc 'pi * 2'".
#include <stdio.h>
#include <string.h>
#include <SWI-Prolog.h>

#define MAXLINE 1024

int main(int argc, char **argv) {
  char expression[MAXLINE];
  char *expressionAddress = expression;
  char *program = argv[0];
  char *argumentVector[2];
  int n;

  // fprintf(stderr, "argc = %d\n", argc);
  if (argc <= 1) {
    fprintf(stderr, "An arithmetic expression must be provided.\n");
    return 1;
  }

  strcpy(expressionAddress, argv[1]);

  // Build Prolog argument vector. 
  argumentVector[0] = program;
  argumentVector[1] = NULL;

  // Attempt to initialize Prolog and halt if it fails.
  if (!PL_initialise(1, argumentVector)) PL_halt(1);

  // Lookup the calc/1 predicate.
  predicate_t predicate = PL_predicate("calc", 1, "user");

  // Create a number of term references, 1 in this case.
  term_t arguments = PL_new_term_refs(1);

  // Add an atom to the list of arguments
  // that is created from the string "expression".
  PL_put_atom_chars(arguments, expression);


  module_t module = NULL;
  int flags = PL_Q_NORMAL;
  int rval = PL_call_predicate(module, flags, predicate, arguments);

  PL_halt(rval ? 0 : 1);

  return 0;
}
