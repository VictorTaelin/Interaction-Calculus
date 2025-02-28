//./../../suptt.md//
//./../../old.c//
//./../memory.h//
//./../types.h//
//./../whnf.h//
//./app_lam.c//
//./app_sup.c//
//./col_lam.c//

#include <stdio.h>
#include "../types.h"
#include "../whnf.h"
#include "../types.h"
#include "../memory.h"
#include "../types.h"

// ! [x,y] = &L{a,b}; k
// ---------------------------- GET-SUP
// ! &L{k0,k1} = k;
// &L{![x0,y0]=a;k0, ![x1,y1]=b;k1}
// x <- &L{x0,x1}
// y <- &L{y0,y1}
Term get_sup(Term get, Term sup) {
  interaction_count++;
  //printf("get_sup\n");
  uint64_t get_loc = TERM_VAL(get);
  uint64_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);

  Term a = heap[sup_loc + 0];
  Term b = heap[sup_loc + 1];
  Term k = heap[get_loc + 1];

  // Create new collapser for the body
  uint64_t col_loc = alloc(1);
  heap[col_loc] = k;

  // Create collapse variables
  Term k0 = make_term(CO0, sup_lab, col_loc);
  Term k1 = make_term(CO1, sup_lab, col_loc);

  // Create superpositions for x and y variables
  uint64_t x_sup_loc = alloc(2);
  uint64_t y_sup_loc = alloc(2);

  // Reuse sup_loc for the first GET
  uint64_t get0_loc = sup_loc;
  // heap[get0_loc + 0] = a; // a is already at sup_loc + 0
  heap[get0_loc + 1] = k0;
  
  // Create second GET node
  uint64_t get1_loc = alloc(2);
  heap[get1_loc + 0] = b;
  heap[get1_loc + 1] = k1;

  // Create variable components for the superpositions
  Term x0 = make_term(VAR, 0, get0_loc + 0);
  Term y0 = make_term(VAR, 0, get0_loc + 1);
  Term x1 = make_term(VAR, 0, get1_loc + 0);
  Term y1 = make_term(VAR, 0, get1_loc + 1);

  // Store superposition components
  heap[x_sup_loc + 0] = x0;
  heap[x_sup_loc + 1] = x1;
  heap[y_sup_loc + 0] = y0;
  heap[y_sup_loc + 1] = y1;

  // Create superposition terms
  Term x_sup = make_term(SUP, sup_lab, x_sup_loc);
  Term y_sup = make_term(SUP, sup_lab, y_sup_loc);

  // Substitute x and y with superpositions
  heap[get_loc + 0] = make_sub(x_sup);
  heap[get_loc + 1] = make_sub(y_sup);

  // Create superposition of GET nodes
  uint64_t new_sup_loc = alloc(2);
  heap[new_sup_loc + 0] = make_term(GET, 0, get0_loc);
  heap[new_sup_loc + 1] = make_term(GET, 0, get1_loc);

  return make_term(SUP, sup_lab, new_sup_loc);
}
