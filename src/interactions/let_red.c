//./../../suptt.md//

#include <stdio.h>
#include "../whnf.h"
#include "../memory.h"
#include "../types.h"

// ! x = t; body
// ------------- LET
// x <- t
// body
Term let_red(Term let) {
  interaction_count++;
  //printf("let_red\n");
  uint64_t let_loc = TERM_VAL(let);

  uint64_t val_loc = let_loc;
  uint64_t bod_loc = let_loc + 1;

  Term val = heap[val_loc];
  Term bod = heap[bod_loc];

  heap[val_loc] = make_sub(val);

  return bod;
}
