//./../../suptt.md//

#include <stdio.h>
#include "../whnf.h"
#include "../memory.h"
#include "../types.h"

// (λx.f a)
// -------- APP-LAM
// x <- a
// f
Term app_lam(Term app, Term lam) {
  interaction_count++;
  printf("app_lam\n");
  uint32_t app_loc = TERM_VAL(app);
  uint32_t lam_loc = TERM_VAL(lam);
  uint32_t arg_loc = app_loc + 1;
  
  Term arg = heap[arg_loc];
  Term bod = heap[lam_loc + 0];
  
  heap[lam_loc] = make_sub(arg);
  
  return bod;
}
