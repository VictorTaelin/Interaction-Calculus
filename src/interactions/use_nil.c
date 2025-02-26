#include "../whnf.h"
#include "../memory.h"

// Implementation of USE-NIL interaction: -(); t -> t
Term use_nil(Term use, Term nil) {
  uint32_t use_loc = TERM_VAL(use);
  
  // Return the body of the use expression
  return heap[use_loc + 1];
}