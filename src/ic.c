/**
 * Interaction Calculus (IC) - Implementation file
 * 
 * This file provides actual implementations for any functions
 * that shouldn't be inlined in the header. Currently, all implementations
 * are in the header file as static inline functions.
 */

#include "ic.h"

// Reduce a term to full normal form by recursively applying WHNF.
// 
// @param ic The IC context
// @param term The term to normalize
// @return The fully normalized term
Term ic_normal(IC* ic, Term term) {
  // Reset stack
  ic->stack_pos = 0;
  Term* heap = ic->heap;
  Term* stack = ic->stack;
  uint32_t stack_pos = 0;

  // Allocate a new node for the initial term
  uint32_t root_loc = ic_alloc(ic, 1);
  heap[root_loc] = term;

  // Push initial location to stack as a "location"
  stack[stack_pos++] = ic_make_term(VAR, root_loc);

  while (stack_pos > 0) {
    // Pop current location from stack
    uint32_t loc = TERM_VAL(stack[--stack_pos]);

    // Get term at this location
    Term current = heap[loc];

    // Reduce to WHNF
    current = ic_whnf(ic, current);

    // Store the WHNF term back to the heap
    heap[loc] = current;

    // Get term details
    TermTag tag = TERM_TAG(current);
    uint32_t val = TERM_VAL(current);

    // Push subterm locations based on term type
    if (tag == LAM) {
      stack[stack_pos++] = ic_make_term(VAR, val + 0);
    } else if (tag == APP || IS_SUP(tag)) {
      stack[stack_pos++] = ic_make_term(VAR, val + 0);
      stack[stack_pos++] = ic_make_term(VAR, val + 1);
    }
  }

  // Update stack position and return the fully normalized term
  ic->stack_pos = stack_pos;
  return heap[root_loc];
}
