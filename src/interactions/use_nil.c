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

// - () t
// ------ USE-NIL
// t
Term use_nil(Term use, Term nil) {
  interaction_count++;
  printf("use_nil\n");
  uint32_t use_loc = TERM_VAL(use);
  
  // The body is the second term in the Use Node
  Term body = heap[use_loc + 1];
  
  return body;
}
