//./../IC.md//
//./types.h//
//./memory.h//

#include <stdio.h>
#include <stdlib.h>
#include "normal.h"
#include "whnf.h"
#include "memory.h"

// Manual stack for normal form reduction
#define NORMAL_STACK_SIZE (1 << 24)
uint32_t normal_stack[NORMAL_STACK_SIZE]; // Stack of heap locations
uint32_t normal_sp = 0;

// Helper functions for stack operations
void normal_push(uint32_t loc) {
  if (normal_sp >= NORMAL_STACK_SIZE) {
    fprintf(stderr, "Stack overflow in normal form reduction\n");
    exit(1);
  }
  normal_stack[normal_sp++] = loc;
}

uint32_t normal_pop() {
  if (normal_sp == 0) {
    fprintf(stderr, "Stack underflow in normal form reduction\n");
    exit(1);
  }
  return normal_stack[--normal_sp];
}

// Reduce a term to full normal form using a stack
Term normal(Term term) {
  // Reset stack
  normal_sp = 0;
  
  // Allocate a new node for the initial term
  uint32_t root_loc = alloc(1);
  heap[root_loc] = term;
  
  // Push initial location to stack
  normal_push(root_loc);
  
  while (normal_sp > 0) {
    // Pop current location from stack
    uint32_t loc = normal_pop();
    
    // Get term at this location
    Term current = heap[loc];
    
    // Reduce to WHNF
    current = whnf(current);
    
    // Store the WHNF term back to the heap
    heap[loc] = current;
    
    // Get term details
    TermTag tag = TERM_TAG(current);
    uint32_t val = TERM_VAL(current);
    
    // Push subterm locations based on term type
    switch (tag) {
      case LAM:
        normal_push(val); // Push body location
        break;
      case APP:
        normal_push(val);     // Push function location
        normal_push(val + 1); // Push argument location
        break;
      case SUP:
        normal_push(val);     // Push left location
        normal_push(val + 1); // Push right location
        break;
      default:
        // No subterms to process for VAR, CO0, CO1
        break;
    }
  }
  
  // Get the fully normalized term
  Term result = heap[root_loc];
  
  // Free the temporary root node
  // Note: Not freeing here as memory management is handled elsewhere
  
  return result;
}
