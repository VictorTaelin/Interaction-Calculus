//./../../suptt.md//
//./../../old.c//
//./../memory.h//
//./../types.h//
//./../whnf.h//
//./app_lam.c//
//./app_sup.c//

#include <stdio.h>
#include "../whnf.h"
#include "../memory.h"

// ! &L{x0,x1} = 0; K
// ------------------ COL-B_0
// x0 <- 0
// x1 <- 0
// K
Term col_b_0(Term col, Term b_0) {
  printf("col_b_0\n");
  uint32_t col_loc = TERM_VAL(col);
  
  heap[col_loc] = make_sub(b_0);
  
  return b_0;
}
