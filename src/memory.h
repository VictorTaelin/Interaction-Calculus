#ifndef MEMORY_H
#define MEMORY_H

#include "types.h"

// The heap (global memory buffer)
extern Term heap[HEAP_SIZE];

// The current allocation pointer
extern uint32_t heap_ptr;

// Initialize the memory system
void init_memory();

// Allocate n consecutive terms in memory
uint32_t alloc(uint32_t n);

// Create a term with the given tag and value
Term make_term(TermTag tag, uint8_t lab, uint32_t val);

// Create a substitution term
Term make_sub(Term term);

// Remove the substitution bit from a term
Term clear_sub(Term term);

// Check if a term has a substitution
bool has_sub(Term term);

#endif // MEMORY_H