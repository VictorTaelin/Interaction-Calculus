#include <stdio.h>
#include "../whnf.h"
#include "../memory.h"

// Implementation of COL-B_1 interaction: !&L{x0,x1}=1; K -> x0<-1; x1<-1; K
Term col_b_1(Term col, Term b_1) {
  printf("col_b_1\n");
  uint32_t col_loc = TERM_VAL(col);
  
  // Store 1 as substitution for the other half of the collapser
  heap[col_loc] = make_sub(b_1);
  
  // Return 1 for this variable
  return b_1;
}
