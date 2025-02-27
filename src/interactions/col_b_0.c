//./../../suptt.md//

#include <stdio.h>
#include "../whnf.h"
#include "../memory.h"
#include "../types.h"

// ! &L{x0,x1} = 0; K
// ------------------ COL-BT0
// x0 <- 0
// x1 <- 0
// K
Term col_b_0(Term col, Term bt0) {
  interaction_count++;
  //printf("col_b_0\n");
  uint64_t col_loc = TERM_VAL(col);

  heap[col_loc] = make_sub(bt0);

  return bt0;
}
