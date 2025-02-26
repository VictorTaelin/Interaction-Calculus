#ifndef SHOW_H
#define SHOW_H

#include "types.h"
#include <stdio.h>

// Convert a term to string representation and write to the given file
void show_term(FILE* file, Term term);

// Get the string representation of a term tag
const char* show_tag(TermTag tag);

#endif // SHOW_H