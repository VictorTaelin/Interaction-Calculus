#include <stdio.h>
#include "../whnf.h"
#include "../memory.h"

// Implementation of GET-SUP interaction:
// ![x,y]=&L{a,b}; k -> !&L{k0,k1}=k; &L{![x,y]=a;k0, ![x,y]=b;k1}
Term get_sup(Term get, Term sup) {
  printf("get_sup\n");
  uint32_t get_loc = TERM_VAL(get);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  
  // Get components
  Term x = heap[get_loc];
  Term y = heap[get_loc + 1];
  Term body = heap[get_loc + 3];
  Term lft = heap[sup_loc];
  Term rgt = heap[sup_loc + 1];
  
  // Create locations for new terms
  uint32_t k0_loc = alloc(1);
  uint32_t k1_loc = alloc(1);
  uint32_t get0_loc = alloc(4);
  uint32_t get1_loc = alloc(4);
  uint32_t new_sup_loc = alloc(2);
  
  // Set up collapsers for body
  heap[k0_loc] = body;
  
  // Create new get expressions
  heap[get0_loc] = x;
  heap[get0_loc + 1] = y;
  heap[get0_loc + 2] = lft;
  heap[get0_loc + 3] = make_term(CO0, sup_lab, k0_loc);
  
  heap[get1_loc] = x;
  heap[get1_loc + 1] = y;
  heap[get1_loc + 2] = rgt;
  heap[get1_loc + 3] = make_term(CO1, sup_lab, k0_loc);
  
  // Create superposition of get expressions
  heap[new_sup_loc] = make_term(GET, 0, get0_loc);
  heap[new_sup_loc + 1] = make_term(GET, 0, get1_loc);
  
  // Return the superposition
  return make_term(SUP, sup_lab, new_sup_loc);
}
