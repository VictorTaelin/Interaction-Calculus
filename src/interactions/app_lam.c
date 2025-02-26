#include "../whnf.h"
#include "../memory.h"

// Implementation of APP-LAM interaction: (λx.f a) -> x <- a; f
Term app_lam(Term app, Term lam) {
  uint32_t app_loc = TERM_VAL(app);
  uint32_t lam_loc = TERM_VAL(lam);
  uint32_t arg_loc = app_loc + 1;
  
  // Get the argument term from application
  Term arg = heap[arg_loc];
  
  // Get lambda body
  Term body = heap[lam_loc];
  
  // Create substitution by storing arg at lam_loc with sub bit set
  heap[lam_loc] = make_sub(arg);
  
  // Return the lambda body
  return body;
}