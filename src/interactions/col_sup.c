#include "../whnf.h"
#include "../memory.h"

// Implementation of COL-SUP interaction: !{x,y}={a,b}; K -> x<-a; y<-b; K
Term col_sup(Term col, Term sup) {
  uint8_t col_lab = TERM_LAB(col);
  uint8_t sup_lab = TERM_LAB(sup);
  uint32_t col_loc = TERM_VAL(col);
  uint32_t sup_loc = TERM_VAL(sup);
  
  // Get superposition parts
  Term lft = heap[sup_loc];
  Term rgt = heap[sup_loc + 1];
  
  if (col_lab == sup_lab) {
    // Same label case: direct substitution
    // Determine which col var we are (CO0 or CO1)
    if (TERM_TAG(col) == CO0) {
      // Store the right side as substitution for the other variable
      heap[col_loc] = make_sub(rgt);
      // Return the left side for this variable
      return lft;
    } else { // CO1
      // Store the left side as substitution for the other variable
      heap[col_loc] = make_sub(lft);
      // Return the right side for this variable
      return rgt;
    }
  } else {
    // Different label case: need to create nested superpositions
    // Allocate new locations for fresh variables and superpositions
    uint32_t a0_loc = alloc(1);
    uint32_t a1_loc = alloc(1);
    uint32_t b0_loc = alloc(1);
    uint32_t b1_loc = alloc(1);
    
    uint32_t sup0_loc = alloc(2);
    uint32_t sup1_loc = alloc(2);
    
    // Create superposition terms with the original label
    heap[sup0_loc] = make_term(CO0, col_lab, a0_loc);
    heap[sup0_loc + 1] = make_term(CO0, col_lab, b0_loc);
    
    heap[sup1_loc] = make_term(CO1, col_lab, a0_loc);
    heap[sup1_loc + 1] = make_term(CO1, col_lab, b0_loc);
    
    // Store superpositions as substitution targets
    heap[a0_loc] = lft;
    heap[b0_loc] = rgt;
    
    // Create the resulting superpositions
    Term su0 = make_term(SUP, sup_lab, sup0_loc);
    Term su1 = make_term(SUP, sup_lab, sup1_loc);
    
    // Store the appropriate superposition as substitution for the collapser
    heap[col_loc] = make_sub(TERM_TAG(col) == CO0 ? su1 : su0);
    
    // Return the appropriate superposition for this variable
    return TERM_TAG(col) == CO0 ? su0 : su1;
  }
}