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

// ! &L{x0,x1} = 1; K
// ------------------ COL-B_1
// x0 <- 1
// x1 <- 1
// K
Term col_b_1(Term col, Term b_1) {
  printf("col_b_1\n");
  uint32_t col_loc = TERM_VAL(col);
  
  heap[col_loc] = make_sub(b_1);
  
  return b_1;
}
