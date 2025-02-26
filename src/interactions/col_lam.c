#include <stdio.h>
#include "../whnf.h"
#include "../memory.h"

// Implementation of COL-LAM interaction:
// !{r,s}=λx.f; K -> r<-λx0.f0; s<-λx1.f1; x<-{x0,x1}; !{f0,f1}=f; K
Term col_lam(Term col, Term lam) {
  printf("col_lam\n");
  uint8_t col_lab = TERM_LAB(col);
  uint32_t col_loc = TERM_VAL(col);
  uint32_t lam_loc = TERM_VAL(lam);
  
  // Get lambda body
  Term body = heap[lam_loc];
  
  // Allocate locations for new variables and terms
  uint32_t x0_loc = alloc(1);
  uint32_t x1_loc = alloc(1);
  uint32_t f0_loc = alloc(1);
  uint32_t f1_loc = alloc(1);
  uint32_t lam0_loc = alloc(1);
  uint32_t lam1_loc = alloc(1);
  uint32_t sup_loc = alloc(2);
  
  // Create new lambda terms
  heap[lam0_loc] = make_term(VAR, 0, f0_loc);
  heap[lam1_loc] = make_term(VAR, 0, f1_loc);
  
  Term lam0 = make_term(LAM, 0, lam0_loc);
  Term lam1 = make_term(LAM, 0, lam1_loc);
  
  // Create superposition of variable
  heap[sup_loc] = make_term(VAR, 0, x0_loc);
  heap[sup_loc + 1] = make_term(VAR, 0, x1_loc);
  Term var_sup = make_term(SUP, col_lab, sup_loc);
  
  // Substitute original lambda variable with superposition
  heap[lam_loc] = make_sub(var_sup);
  
  // Set up collapser for f0,f1
  heap[f0_loc] = body;
  heap[f1_loc] = body;
  
  // Store appropriate lambda in collapser location based on which col var we are
  if (TERM_TAG(col) == CO0) {
    heap[col_loc] = make_sub(lam1);
    return lam0;
  } else { // CO1
    heap[col_loc] = make_sub(lam0);
    return lam1;
  }
}
