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
#include "../types.h"
#include "../whnf.h"
#include "../types.h"
#include "../memory.h"
#include "../types.h"

// % θ; t
// ------ RWT-RFL
// t
Term rwt_rfl(Term rwt, Term rfl) {
  interaction_count++;
  printf("rwt_rfl\n");
  uint32_t rwt_loc = TERM_VAL(rwt);
  
  Term body = heap[rwt_loc + 1];
  
  return body;
}
