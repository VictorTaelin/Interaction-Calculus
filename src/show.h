//./../InteractionCalculus.md//
//./show.c//

#ifndef SHOW_H
#define SHOW_H

#include <stdio.h>
#include "hvmn.h"

// Convert a term to its string representation
// The returned string is dynamically allocated and must be freed by the caller
char* term_to_string(HVMN* hvmn, Term term);

// Display a term to the specified output stream
void show_term(FILE* stream, HVMN* hvmn, Term term);

#endif // SHOW_H
