//./../../InteractionCalculus.md//
//./../../old.c//
//./../memory.h//
//./../types.h//
//./../whnf.h//
//./app_lam.c//
//./app_sup.c//
//./col_lam.c//

#include "../memory.h"
#include "../types.h"
#include "../whnf.h"
#include "../types.h"
#include <stdio.h>
#include "../types.h"

// ! &L{x,y} = &L{a,b}; K  (if equal labels)
// -------------------- COL-SUP
// x <- a
// y <- b
// K
//
// ! &L{x,y} = &R{a,b}; K  (if different labels)
// -------------------- COL-SUP
// x <- &R{a0,b0} 
// y <- &R{a1,b1}
// ! &L{a0,a1} = a
// ! &L{b0,b1} = b
// K
Term col_sup(Term col, Term sup) {
  interaction_count++;
  //printf("col_sup\n");
  uint32_t col_loc = TERM_VAL(col);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t col_lab = TERM_LAB(col);
  uint8_t sup_lab = TERM_LAB(sup);
  uint8_t is_co0 = TERM_TAG(col) == CO0;

  Term lft = heap[sup_loc + 0];
  Term rgt = heap[sup_loc + 1];

  if (col_lab == sup_lab) {
    // Labels match: simple substitution
    if (is_co0) {
      heap[col_loc] = make_sub(rgt);
      return lft;
    } else {
      heap[col_loc] = make_sub(lft);
      return rgt;
    }
  } else {
    // Labels don't match: create nested collapsers
    // Instead of allocating new memory for collapser locations,
    // directly use the values at sup_loc + 0 and sup_loc + 1
    // The values at these locations (lft and rgt) are already loaded
    uint32_t col_lft_loc = sup_loc + 0;
    uint32_t col_rgt_loc = sup_loc + 1;

    // We don't need to set heap[col_lft_loc] = lft and heap[col_rgt_loc] = rgt
    // because lft is already at sup_loc + 0 and rgt is already at sup_loc + 1

    uint32_t sup0_loc = alloc(2);
    heap[sup0_loc + 0] = make_term(CO0, col_lab, col_lft_loc);
    heap[sup0_loc + 1] = make_term(CO0, col_lab, col_rgt_loc);

    uint32_t sup1_loc = alloc(2);
    heap[sup1_loc + 0] = make_term(CO1, col_lab, col_lft_loc);
    heap[sup1_loc + 1] = make_term(CO1, col_lab, col_rgt_loc);

    if (is_co0) {
      heap[col_loc] = make_sub(make_term(SUP, sup_lab, sup1_loc));
      return make_term(SUP, sup_lab, sup0_loc);
    } else {
      heap[col_loc] = make_sub(make_term(SUP, sup_lab, sup0_loc));
      return make_term(SUP, sup_lab, sup1_loc);
    }
  }
}
