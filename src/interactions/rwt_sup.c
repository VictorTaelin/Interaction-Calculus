//./../../suptt.md//
//./../../old.c//
//./../memory.h//
//./../types.h//
//./../whnf.h//
//./app_lam.c//
//./app_sup.c//
//./col_lam.c//

// TODO: rwt_sup.c

#include <stdio.h>
#include "../types.h"
#include "../whnf.h"
#include "../types.h"
#include "../memory.h"
#include "../types.h"

// % &L{a,b}; k
// ---------------- RWT-SUP
// ! &L{k0,k1} = k;
// &L{%a;k0, %b;k1}
Term rwt_sup(Term rwt, Term sup) {
  interaction_count++;
  //printf("rwt_sup\n");
  uint64_t rwt_loc = TERM_VAL(rwt);
  uint64_t sup_loc = TERM_VAL(sup);
  uint8_t  sup_lab = TERM_LAB(sup);

  Term bod = heap[rwt_loc + 1]; // The body of the rewrite
  // Term lft = heap[sup_loc + 0]; // Left side of superposition - already in heap[sup_loc + 0]
  Term rgt = heap[sup_loc + 1]; // Right side of superposition

  // Create a collapser for the rewrite body
  uint64_t col_loc = alloc(1);
  heap[col_loc] = bod;

  // Create the collapser variables
  Term k0 = make_term(CO0, sup_lab, col_loc);
  Term k1 = make_term(CO1, sup_lab, col_loc);

  // Reuse sup_loc for the first rewrite
  uint64_t rwt0_loc = sup_loc;
  // heap[rwt0_loc + 0] = lft; // Not needed as lft is already in heap[sup_loc + 0]
  heap[rwt0_loc + 1] = k0;

  uint64_t rwt1_loc = alloc(2);
  heap[rwt1_loc + 0] = rgt;
  heap[rwt1_loc + 1] = k1;

  // Reuse rwt_loc for the resulting superposition
  uint64_t sup_new_loc = rwt_loc;
  heap[sup_new_loc + 0] = make_term(RWT, 0, rwt0_loc);
  heap[sup_new_loc + 1] = make_term(RWT, 0, rwt1_loc);

  return make_term(SUP, sup_lab, sup_new_loc);
}
