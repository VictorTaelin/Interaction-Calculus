#include "../whnf.h"
#include "../memory.h"

// Implementation of COL-NIL interaction: !&L{x0,x1}=(); K -> x0<-(); x1<-(); K
Term col_nil(Term col, Term nil) {
  uint32_t col_loc = TERM_VAL(col);
  
  // Store () as substitution for the other half of the collapser
  heap[col_loc] = make_sub(nil);
  
  // Return () for this variable
  return nil;
}