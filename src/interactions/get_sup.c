//./../../suptt.md//
//./../../old.c//
//./../memory.h//
//./../types.h//
//./../whnf.h//
//./app_lam.c//
//./app_sup.c//
//./col_lam.c//

#include <stdio.h>
#include "../whnf.h"
#include "../memory.h"

// ! [x,y] = &L{a,b}; k
// ---------------------------- GET-SUP
// ! &L{k0,k1} = k;
// &L{![x,y]=a;k0, ![x,y]=b;k1}
Term get_sup(Term get, Term sup) {
  printf("get_sup\n");
  uint32_t get_loc = TERM_VAL(get);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  
  Term a = heap[sup_loc + 0];
  Term b = heap[sup_loc + 1];
  Term k = heap[get_loc + 1];
  
  // Create a new collapser for the body
  uint32_t col_loc = alloc(1);
  heap[col_loc] = k;
  
  // Create collapse variables
  Term k0 = make_term(CO0, sup_lab, col_loc);
  Term k1 = make_term(CO1, sup_lab, col_loc);
  
  // Create two new GET eliminators
  uint32_t get0_loc = alloc(2);
  heap[get0_loc + 0] = a;
  heap[get0_loc + 1] = k0;
  
  uint32_t get1_loc = alloc(2);
  heap[get1_loc + 0] = b;
  heap[get1_loc + 1] = k1;
  
  // Create a new superposition
  uint32_t new_sup_loc = alloc(2);
  heap[new_sup_loc + 0] = make_term(GET, 0, get0_loc);
  heap[new_sup_loc + 1] = make_term(GET, 0, get1_loc);
  
  return make_term(SUP, sup_lab, new_sup_loc);
}
