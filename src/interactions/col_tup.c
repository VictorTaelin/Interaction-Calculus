#include "../whnf.h"
#include "../memory.h"

// Implementation of COL-TUP interaction:
// !&L{x0,x1}=[a,b]; K -> x0<-[a0,b0]; x1<-[a1,b1]; !&L{a0,a1}=a; !&L{b0,b1}=b; K
Term col_tup(Term col, Term tup) {
  uint8_t col_lab = TERM_LAB(col);
  uint32_t col_loc = TERM_VAL(col);
  uint32_t tup_loc = TERM_VAL(tup);
  
  // Get tuple components
  Term fst = heap[tup_loc];
  Term snd = heap[tup_loc + 1];
  
  // Create new locations for components and tuples
  uint32_t a0_loc = alloc(1);
  uint32_t a1_loc = alloc(1);
  uint32_t b0_loc = alloc(1);
  uint32_t b1_loc = alloc(1);
  uint32_t tup0_loc = alloc(2);
  uint32_t tup1_loc = alloc(2);
  
  // Set up collapsers for components
  heap[a0_loc] = fst;
  heap[b0_loc] = snd;
  
  // Create new tuples
  heap[tup0_loc] = make_term(CO0, col_lab, a0_loc);
  heap[tup0_loc + 1] = make_term(CO0, col_lab, b0_loc);
  
  heap[tup1_loc] = make_term(CO1, col_lab, a0_loc);
  heap[tup1_loc + 1] = make_term(CO1, col_lab, b0_loc);
  
  Term tup0 = make_term(TUP, 0, tup0_loc);
  Term tup1 = make_term(TUP, 0, tup1_loc);
  
  // Store the appropriate tuple as substitution based on which col var we are
  heap[col_loc] = make_sub(TERM_TAG(col) == CO0 ? tup1 : tup0);
  
  // Return the appropriate tuple for this variable
  return TERM_TAG(col) == CO0 ? tup0 : tup1;
}