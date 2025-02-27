//./../../suptt.md//

#include <stdio.h>
#include "../whnf.h"
#include "../memory.h"
#include "../types.h"

// ({a,b} c)
// --------------- APP-SUP
// ! {x0,x1} = c;
// {(a x0),(b x1)}
Term app_sup(Term app, Term sup) {
  interaction_count++;
  //printf("app_sup\n");
  uint64_t app_loc = TERM_VAL(app);
  uint64_t sup_loc = TERM_VAL(sup);
  uint8_t  sup_lab = TERM_LAB(sup);

  Term arg = heap[app_loc + 1];
  Term lft = heap[sup_loc + 0];
  Term rgt = heap[sup_loc + 1];

  uint64_t col_loc = alloc(1);
  heap[col_loc] = arg;

  Term x0 = make_term(CO0, sup_lab, col_loc);
  Term x1 = make_term(CO1, sup_lab, col_loc);

  // Instead of allocating new memory for app0, reuse sup_loc
  uint64_t app0_loc = sup_loc;
  // lft is already in heap[app0_loc + 0]
  heap[app0_loc + 1] = x0;

  uint64_t app1_loc = alloc(2);
  heap[app1_loc + 0] = rgt;
  heap[app1_loc + 1] = x1;

  // Instead of allocating new memory for sup_new, reuse app_loc
  uint64_t sup_new_loc = app_loc;
  heap[sup_new_loc + 0] = make_term(APP, 0, app0_loc);
  heap[sup_new_loc + 1] = make_term(APP, 0, app1_loc);

  return make_term(SUP, sup_lab, sup_new_loc);
}
