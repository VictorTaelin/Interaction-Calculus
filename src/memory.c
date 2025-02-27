#include <stdio.h>
#include <stdlib.h>
#include "memory.h"

// The heap (global memory buffer)
Term heap[HEAP_SIZE];

// The current allocation pointer
uint32_t heap_ptr = 0;

// Initialize the memory system
void init_memory() {
  heap_ptr = 0;
  for (uint32_t i = 0; i < HEAP_SIZE; i++) {
    heap[i] = 0;
  }
}

// Allocate n consecutive terms in memory
uint32_t alloc(uint32_t n) {
  if (heap_ptr + n >= HEAP_SIZE) {
    fprintf(stderr, "Error: Out of memory\n");
    exit(1);
  }

  uint32_t ptr = heap_ptr;
  heap_ptr += n;
  return ptr;
}

// Create a term with the given tag and value
Term make_term(TermTag tag, uint8_t lab, uint32_t val) {
  return MAKE_TERM(false, tag, lab, val);
}

// Create a substitution term
Term make_sub(Term term) {
  return MAKE_TERM(true, TERM_TAG(term), TERM_LAB(term), TERM_VAL(term));
}

// Remove the substitution bit from a term
Term clear_sub(Term term) {
  return MAKE_TERM(false, TERM_TAG(term), TERM_LAB(term), TERM_VAL(term));
}

// Check if a term is a substitution
bool has_sub(Term term) {
  return TERM_SUB(term);
}
