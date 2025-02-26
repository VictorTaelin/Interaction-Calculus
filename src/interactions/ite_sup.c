#include "../whnf.h"
#include "../memory.h"

// Implementation of ITE-SUP interaction:
// ?&L{a,b}{t};{f} -> !&L{t0,t1}=t; !&L{f0,f1}=f; &L{?a{t0};{f0}, ?b{t1};{f1}}
Term ite_sup(Term ite, Term sup) {
  uint32_t ite_loc = TERM_VAL(ite);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  
  // Get components
  Term lft = heap[sup_loc];
  Term rgt = heap[sup_loc + 1];
  Term thn = heap[ite_loc + 1];
  Term els = heap[ite_loc + 2];
  
  // Create locations for new terms
  uint32_t t0_loc = alloc(1);
  uint32_t t1_loc = alloc(1);
  uint32_t f0_loc = alloc(1);
  uint32_t f1_loc = alloc(1);
  uint32_t ite0_loc = alloc(3);
  uint32_t ite1_loc = alloc(3);
  uint32_t new_sup_loc = alloc(2);
  
  // Set up collapsers
  heap[t0_loc] = thn;
  heap[f0_loc] = els;
  
  // Create new ite expressions
  heap[ite0_loc] = lft;
  heap[ite0_loc + 1] = make_term(CO0, sup_lab, t0_loc);
  heap[ite0_loc + 2] = make_term(CO0, sup_lab, f0_loc);
  
  heap[ite1_loc] = rgt;
  heap[ite1_loc + 1] = make_term(CO1, sup_lab, t0_loc);
  heap[ite1_loc + 2] = make_term(CO1, sup_lab, f0_loc);
  
  // Create superposition of ite expressions
  heap[new_sup_loc] = make_term(ITE, 0, ite0_loc);
  heap[new_sup_loc + 1] = make_term(ITE, 0, ite1_loc);
  
  // Return the superposition
  return make_term(SUP, sup_lab, new_sup_loc);
}