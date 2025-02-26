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

// ! &L{x0,x1} = (); K
// ------------------- COL-NIL
// x0 <- ()
// x1 <- ()
// K
Term col_nil(Term col, Term nil) {
  interaction_count++;
  printf("col_nil\n");
  uint32_t col_loc = TERM_VAL(col);
  
  // Create the nil term to be returned and for substitution
  Term nil_term = make_term(NIL, 0, 0);
  
  // Substitute the appropriate collapser variable with nil
  heap[col_loc] = make_sub(nil_term);
  
  // Return nil
  return nil_term;
}
