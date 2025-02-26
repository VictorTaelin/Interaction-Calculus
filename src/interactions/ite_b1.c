#include "../whnf.h"
#include "../memory.h"

// Implementation of ITE-B1 interaction: ?1{t};{f} -> t
Term ite_b1(Term ite, Term b_1) {
  uint32_t ite_loc = TERM_VAL(ite);
  
  // Return the then branch
  return heap[ite_loc + 1];
}