//./../../suptt.md//
//./../../old.c//
//./../memory.h//
//./../types.h//
//./../whnf.h//
//./app_lam.c//
//./app_sup.c//
//./col_lam.c//

// TODO: ite_sup.c

#include <stdio.h>
#include "../types.h"
#include "../whnf.h"
#include "../types.h"
#include "../memory.h"
#include "../types.h"

// ? &L{a,b} {t} ; {f}
// ---------------------------- ITE-SUP
// ! &L{t0,t1} = t;
// ! &L{f0,f1} = f;
// &L{?a{t0};{f0}, ?b{t1};{f1}}
Term ite_sup(Term ite, Term sup) {
  interaction_count++;
  //printf("ite_sup\n");
  uint64_t ite_loc = TERM_VAL(ite);
  uint64_t sup_loc = TERM_VAL(sup);
  uint8_t  sup_lab = TERM_LAB(sup);

  Term thn = heap[ite_loc + 1];
  Term els = heap[ite_loc + 2];
  Term lft = heap[sup_loc + 0];
  Term rgt = heap[sup_loc + 1];

  // Create collapsers for then and else branches
  uint64_t col_thn_loc = alloc(1);
  uint64_t col_els_loc = alloc(1);
  heap[col_thn_loc] = thn;
  heap[col_els_loc] = els;

  // Create variables for the collapsers
  Term t0 = make_term(CO0, sup_lab, col_thn_loc);
  Term t1 = make_term(CO1, sup_lab, col_thn_loc);
  Term f0 = make_term(CO0, sup_lab, col_els_loc);
  Term f1 = make_term(CO1, sup_lab, col_els_loc);

  // Reuse ite_loc for the first ITE (it's already sized correctly for ITE)
  uint64_t ite0_loc = ite_loc;
  heap[ite0_loc + 0] = lft;
  heap[ite0_loc + 1] = t0;
  heap[ite0_loc + 2] = f0;

  uint64_t ite1_loc = alloc(3);
  heap[ite1_loc + 0] = rgt;
  heap[ite1_loc + 1] = t1;
  heap[ite1_loc + 2] = f1;

  // Reuse sup_loc for the new superposition
  uint64_t sup_new_loc = sup_loc;
  heap[sup_new_loc + 0] = make_term(ITE, 0, ite0_loc);
  heap[sup_new_loc + 1] = make_term(ITE, 0, ite1_loc);

  return make_term(SUP, sup_lab, sup_new_loc);
}
