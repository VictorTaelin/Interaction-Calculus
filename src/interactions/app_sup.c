#include "../whnf.h"
#include "../memory.h"

// Implementation of APP-SUP interaction: ({a,b} c) -> !{x0,x1}=c; {(a x0),(b x1)}
Term app_sup(Term app, Term sup) {
  uint32_t app_loc = TERM_VAL(app);
  uint32_t sup_loc = TERM_VAL(sup);
  uint32_t arg_loc = app_loc + 1;
  uint8_t sup_lab = TERM_LAB(sup);
  
  // Get the argument and superposition terms
  Term arg = heap[arg_loc];
  Term lft = heap[sup_loc];
  Term rgt = heap[sup_loc + 1];
  
  // Create fresh variables x0, x1
  uint32_t x0_loc = alloc(1);
  uint32_t x1_loc = alloc(1);
  
  // Create applications (a x0) and (b x1)
  uint32_t app0_loc = alloc(2);
  uint32_t app1_loc = alloc(2);
  
  heap[app0_loc] = lft;
  heap[app0_loc + 1] = make_term(VAR, 0, x0_loc);
  
  heap[app1_loc] = rgt;
  heap[app1_loc + 1] = make_term(VAR, 0, x1_loc);
  
  // Create the superposition {(a x0),(b x1)}
  uint32_t new_sup_loc = alloc(2);
  heap[new_sup_loc] = make_term(APP, 0, app0_loc);
  heap[new_sup_loc + 1] = make_term(APP, 0, app1_loc);
  
  // Create collapser variables
  Term co0 = make_term(CO0, sup_lab, x0_loc);
  Term co1 = make_term(CO1, sup_lab, x1_loc);
  
  // Store the argument as substitution for the collapse
  heap[x0_loc] = make_sub(arg);
  
  // Return the final term: !{x0,x1}=c; {(a x0),(b x1)}
  // (Note: The collapse is implicit since we've set up the variables and stored the substitution)
  return make_term(SUP, sup_lab, new_sup_loc);
}