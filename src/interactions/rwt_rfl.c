//./../../suptt.md//
//./../../old.c//
//./../memory.h//
//./../types.h//
//./../whnf.h//
//./app_lam.c//
//./app_sup.c//
//./col_lam.c//

// TODO: rwt_rfl.c

#include <stdio.h>
#include "../whnf.h"
#include "../memory.h"

// % θ; t
// ------ RWT-RFL
// t
Term rwt_rfl(Term rwt, Term rfl) {
  printf("rwt_rfl\n");
  uint32_t rwt_loc = TERM_VAL(rwt);
  
  Term body = heap[rwt_loc + 1];
  
  return body;
}
