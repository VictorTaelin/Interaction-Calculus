//./../InteractionCalculus.md//
//./memory.h//
//./types.h//
//./show.c//

#ifndef SHOW_H
#define SHOW_H

#include <stdio.h>
#include "types.h"

// Convert a term to its string representation
// The returned string is dynamically allocated and must be freed by the caller
char* term_to_string(Term term);

// Display a term to the specified output stream
void show_term(FILE* stream, Term term);

#endif // SHOW_H
