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

// - &L{a,b}; k
// -------------------- USE-SUP
// ! &L{k0,k1} = k;
// &L{-a;k0, -b;k1}
Term use_sup(Term use, Term sup) {
  interaction_count++;
  printf("use_sup\n");
  uint32_t use_loc = TERM_VAL(use);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t  sup_lab = TERM_LAB(sup);

  Term a = heap[sup_loc + 0];
  Term b = heap[sup_loc + 1];
  Term k = heap[use_loc + 1];

  // Create a collapser for k
  uint32_t col_loc = alloc(1);
  heap[col_loc] = k;

  // Create new USE nodes for each branch
  uint32_t use_a_loc = alloc(2);
  heap[use_a_loc + 0] = a;
  heap[use_a_loc + 1] = make_term(CO0, sup_lab, col_loc);

  uint32_t use_b_loc = alloc(2);
  heap[use_b_loc + 0] = b;
  heap[use_b_loc + 1] = make_term(CO1, sup_lab, col_loc);

  // Create new superposition with the USE nodes
  uint32_t new_sup_loc = alloc(2);
  heap[new_sup_loc + 0] = make_term(USE, 0, use_a_loc);
  heap[new_sup_loc + 1] = make_term(USE, 0, use_b_loc);

  return make_term(SUP, sup_lab, new_sup_loc);
}
