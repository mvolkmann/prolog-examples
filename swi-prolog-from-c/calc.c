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
  char *e = expression;
  char *program = argv[0];
  char *plav[2];
  int n;

  // fprintf(stderr, "argc = %d\n", argc);
  if (argc <= 1) {
    fprintf(stderr, "An arithmetic expression must be provided.\n");
    return 1;
  }

  strcpy(e, argv[1]);

  // Build Prolog argument vector (plav). 
  plav[0] = program;
  plav[1] = NULL;

  // Attempt to initialize Prolog and halt if it fails.
  if (!PL_initialise(1, plav)) PL_halt(1);

  // Lookup the calc/1 predicate.
  predicate_t pred = PL_predicate("calc", 1, "user");
  term_t h0 = PL_new_term_refs(1);

  PL_put_atom_chars(h0, expression);
  int rval = PL_call_predicate(NULL, PL_Q_NORMAL, pred, h0);

  PL_halt(rval ? 0 : 1);

  return 0;
}
