#include <stdio.h>
#include "../whnf.h"
#include "../memory.h"

// Implementation of COL-B_0 interaction: !&L{x0,x1}=0; K -> x0<-0; x1<-0; K
Term col_b_0(Term col, Term b_0) {
  printf("col_b_0\n");
  uint32_t col_loc = TERM_VAL(col);
  
  // Store 0 as substitution for the other half of the collapser
  heap[col_loc] = make_sub(b_0);
  
  // Return 0 for this variable
  return b_0;
}
