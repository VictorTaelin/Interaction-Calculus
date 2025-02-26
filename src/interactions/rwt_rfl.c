#include "../whnf.h"
#include "../memory.h"

// Implementation of RWT-RFL interaction: %θ; t -> t
Term rwt_rfl(Term rwt, Term rfl) {
  uint32_t rwt_loc = TERM_VAL(rwt);
  
  // Return the body
  return heap[rwt_loc + 1];
}