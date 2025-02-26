#include "../whnf.h"
#include "../memory.h"

// Implementation of ITE-B0 interaction: ?0{t};{f} -> f
Term ite_b_0(Term ite, Term b_0) {
  uint32_t ite_loc = TERM_VAL(ite);
  
  // Return the else branch
  return heap[ite_loc + 2];
}
