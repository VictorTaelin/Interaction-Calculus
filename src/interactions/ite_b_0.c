//./../../suptt.md//
//./../../old.c//
//./../memory.h//
//./../types.h//
//./../whnf.h//
//./app_lam.c//
//./app_sup.c//
//./col_lam.c//

// TODO: ite_b_0.c

#include <stdio.h>
#include "../types.h"
#include "../whnf.h"
#include "../types.h"
#include "../memory.h"
#include "../types.h"

// ? 0 { t } ; { f }
// ----------------- ITE-BT0
// f
Term ite_b_0(Term ite, Term bt0) {
  interaction_count++;
  //printf("ite_b_0\n");
  uint64_t ite_loc = TERM_VAL(ite);

  // ITE structure: {cnd, thn, els}
  // We need to return the else branch
  Term els = heap[ite_loc + 2];

  return els;
}
