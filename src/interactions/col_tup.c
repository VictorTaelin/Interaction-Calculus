//./../../suptt.md//
//./../../old.c//
//./../memory.h//
//./../types.h//
//./../whnf.h//
//./app_lam.c//
//./app_sup.c//
//./col_lam.c//

#include <stdio.h>
#include "../types.h"
#include "../whnf.h"
#include "../types.h"
#include "../memory.h"
#include "../types.h"

// ! &L{x0,x1} = [a,b]; K
// ---------------------- COL-TUP
// x0 <- [a0,b0]
// x1 <- [a1,b1]
// ! &L{a0,a1} = a
// ! &L{b0,b1} = b
// K
Term col_tup(Term col, Term tup) {
  interaction_count++;
  //printf("col_tup\n");
  uint64_t col_loc = TERM_VAL(col);
  uint64_t tup_loc = TERM_VAL(tup);
  uint8_t col_lab = TERM_LAB(col);
  uint8_t is_co0 = TERM_TAG(col) == CO0;

  // Use the tuple locations directly instead of copying
  uint64_t col_fst_loc = tup_loc + 0;
  uint64_t col_snd_loc = tup_loc + 1;

  // Create new variables for the collapsers
  Term a0 = make_term(CO0, col_lab, col_fst_loc);
  Term a1 = make_term(CO1, col_lab, col_fst_loc);
  Term b0 = make_term(CO0, col_lab, col_snd_loc);
  Term b1 = make_term(CO1, col_lab, col_snd_loc);

  // Reuse tup_loc for one of the tuples
  uint64_t tup0_loc = tup_loc;
  uint64_t tup1_loc = alloc(2);
  
  // Store a0, b0 in first tuple (reusing original tuple memory)
  heap[tup0_loc + 0] = a0;
  heap[tup0_loc + 1] = b0;
  
  // Store a1, b1 in second tuple
  heap[tup1_loc + 0] = a1;
  heap[tup1_loc + 1] = b1;

  // Create the new tuples
  Term tup0 = make_term(TUP, 0, tup0_loc);
  Term tup1 = make_term(TUP, 0, tup1_loc);

  // Substitute the variables
  if (is_co0) {
    heap[col_loc] = make_sub(tup1);
    return tup0;
  } else {
    heap[col_loc] = make_sub(tup0);
    return tup1;
  }
}
