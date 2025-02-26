//./../../suptt.md//
//./../../old.c//
//./../memory.h//
//./../types.h//
//./../whnf.h//

#include <stdio.h>
#include "../whnf.h"
#include "../memory.h"

// ! &L{r,s} = λx.f;
// K
// ----------------- COL-LAM
// r <- λx0.f0
// s <- λx1.f1
// x <- &L{x0,x1}
// ! &L{f0,f1} = f;
// K
Term col_lam(Term col, Term lam) {
  printf("col_lam\n");
  uint32_t col_loc = TERM_VAL(col);
  uint32_t lam_loc = TERM_VAL(lam);
  uint8_t col_lab = TERM_LAB(col);
  uint8_t is_co0 = TERM_TAG(col) == CO0;
  
  Term bod = heap[lam_loc + 0];
  
  // Create new lambda variables
  uint32_t lam0_loc = alloc(1);
  uint32_t lam1_loc = alloc(1);
  
  // Create a superposition for lambda's variable
  uint32_t sup_loc = alloc(2);
  heap[sup_loc + 0] = make_term(VAR, 0, lam0_loc);
  heap[sup_loc + 1] = make_term(VAR, 0, lam1_loc);
  
  // Replace lambda's variable with the superposition
  heap[lam_loc] = make_sub(make_term(SUP, col_lab, sup_loc));
  
  // Create a collapser for lambda's body
  uint32_t col_new_loc = alloc(1);
  heap[col_new_loc] = bod;
  
  // Set up new lambda bodies
  heap[lam0_loc] = make_term(CO0, col_lab, col_new_loc);
  heap[lam1_loc] = make_term(CO1, col_lab, col_new_loc);
  
  // Create and return the appropriate lambda
  Term new_lam;
  if (is_co0) {
    heap[col_loc] = make_sub(make_term(LAM, 0, lam1_loc));
    new_lam = make_term(LAM, 0, lam0_loc);
  } else {
    heap[col_loc] = make_sub(make_term(LAM, 0, lam0_loc));
    new_lam = make_term(LAM, 0, lam1_loc);
  }
  
  return new_lam;
}

