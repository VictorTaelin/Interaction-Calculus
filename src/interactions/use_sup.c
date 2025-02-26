#include "../whnf.h"
#include "../memory.h"

// Implementation of USE-SUP interaction: -&L{a,b}; k -> !&L{k0,k1}=k; &L{-a;k0, -b;k1}
Term use_sup(Term use, Term sup) {
  uint32_t use_loc = TERM_VAL(use);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  
  // Get the body of use and components of superposition
  Term body = heap[use_loc + 1];
  Term lft = heap[sup_loc];
  Term rgt = heap[sup_loc + 1];
  
  // Create locations for new terms
  uint32_t k0_loc = alloc(1);
  uint32_t k1_loc = alloc(1);
  uint32_t use0_loc = alloc(2);
  uint32_t use1_loc = alloc(2);
  uint32_t new_sup_loc = alloc(2);
  
  // Store body in collapsers
  heap[k0_loc] = body;
  
  // Create new use expressions
  heap[use0_loc] = lft;
  heap[use0_loc + 1] = make_term(CO0, sup_lab, k0_loc);
  
  heap[use1_loc] = rgt;
  heap[use1_loc + 1] = make_term(CO1, sup_lab, k0_loc);
  
  // Create superposition of use expressions
  heap[new_sup_loc] = make_term(USE, 0, use0_loc);
  heap[new_sup_loc + 1] = make_term(USE, 0, use1_loc);
  
  // Return the superposition
  return make_term(SUP, sup_lab, new_sup_loc);
}