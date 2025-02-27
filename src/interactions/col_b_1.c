//./../../suptt.md//

#include <stdio.h>
#include "../whnf.h"
#include "../memory.h"
#include "../types.h"

// ! &L{x0,x1} = 1; K
// ------------------ COL-BT1
// x0 <- 1
// x1 <- 1
// K
Term col_b_1(Term col, Term bt1) {
  interaction_count++;
  //printf("col_b_1\n");
  uint64_t col_loc = TERM_VAL(col);

  heap[col_loc] = make_sub(bt1);

  return bt1;
}
