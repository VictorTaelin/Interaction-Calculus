//./../../suptt.md//
//./../../old.c//
//./../memory.h//
//./../types.h//
//./../whnf.h//
//./app_lam.c//
//./app_sup.c//
//./col_lam.c//

// TODO: ite_b_1.c

#include <stdio.h>
#include "../types.h"
#include "../whnf.h"
#include "../types.h"
#include "../memory.h"
#include "../types.h"

// ? 1 { t } ; { f }
// ----------------- ITE-B_1
// t
Term ite_b_1(Term ite, Term b_1) {
  interaction_count++;
  printf("ite_b_1\n");
  uint32_t ite_loc = TERM_VAL(ite);
  
  // ITE structure: {cnd, thn, els}
  // We need to return the then branch
  Term thn = heap[ite_loc + 1];
  
  return thn;
}
