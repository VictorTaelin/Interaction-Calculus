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

// - () t
// ------ USE-NIL
// t
Term use_nil(Term use, Term nil) {
  printf("use_nil\n");
  uint32_t use_loc = TERM_VAL(use);
  
  // The body is the second term in the Use Node
  Term body = heap[use_loc + 1];
  
  return body;
}
