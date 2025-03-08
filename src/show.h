//./../InteractionCalculus.md//
//./show.c//

#ifndef SHOW_H
#define SHOW_H

#include <stdio.h>
#include "ic.h"

// Convert a term to its string representation
// The returned string is dynamically allocated and must be freed by the caller
char* term_to_string(IC* ic, Term term);

// Convert a term to its string representation with a prefix for variable names
// The returned string is dynamically allocated and must be freed by the caller
char* term_to_string_namespaced(IC* ic, Term term, const char* prefix);

// Display a term to the specified output stream
void show_term(FILE* stream, IC* ic, Term term);

// Display a term to the specified output stream with a prefix for variable names
void show_term_namespaced(FILE* stream, IC* ic, Term term, const char* prefix);

#endif // SHOW_H
