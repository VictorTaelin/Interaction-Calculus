#include "../whnf.h"
#include "../memory.h"

// Implementation of RWT-SUP interaction: %&L{a,b}; k -> !&L{k0,k1}=k; &L{%a;k0, %b;k1}
Term rwt_sup(Term rwt, Term sup) {
  uint32_t rwt_loc = TERM_VAL(rwt);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  
  // Get components
  Term body = heap[rwt_loc + 1];
  Term lft = heap[sup_loc];
  Term rgt = heap[sup_loc + 1];
  
  // Create locations for new terms
  uint32_t k0_loc = alloc(1);
  uint32_t k1_loc = alloc(1);
  uint32_t rwt0_loc = alloc(2);
  uint32_t rwt1_loc = alloc(2);
  uint32_t new_sup_loc = alloc(2);
  
  // Set up collapsers for body
  heap[k0_loc] = body;
  
  // Create new rwt expressions
  heap[rwt0_loc] = lft;
  heap[rwt0_loc + 1] = make_term(CO0, sup_lab, k0_loc);
  
  heap[rwt1_loc] = rgt;
  heap[rwt1_loc + 1] = make_term(CO1, sup_lab, k0_loc);
  
  // Create superposition of rwt expressions
  heap[new_sup_loc] = make_term(RWT, 0, rwt0_loc);
  heap[new_sup_loc + 1] = make_term(RWT, 0, rwt1_loc);
  
  // Return the superposition
  return make_term(SUP, sup_lab, new_sup_loc);
}