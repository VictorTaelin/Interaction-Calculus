//./../../suptt.md//
//./../../old.c//
//./../memory.h//
//./../types.h//
//./../whnf.h//
//./app_lam.c//
//./app_sup.c//
//./col_lam.c//

// TODO: get_tup

#include <stdio.h>
#include "../types.h"
#include "../whnf.h"
#include "../types.h"
#include "../memory.h"
#include "../types.h"

// ! [x,y] = [a,b]; t
// ------------------ GET-TUP
// x <- a
// y <- b
// t
Term get_tup(Term get, Term tup) {
  interaction_count++;
  printf("get_tup\n");
  uint32_t get_loc = TERM_VAL(get);
  uint32_t tup_loc = TERM_VAL(tup);
  
  // Extract tuple values
  Term a = heap[tup_loc + 0];
  Term b = heap[tup_loc + 1];
  
  // Extract GET body
  Term bod = heap[get_loc + 1];
  
  // Update locations for GET variables with substitutions
  heap[get_loc + 2] = make_sub(a); // Substitute a for x
  heap[get_loc + 3] = make_sub(b); // Substitute b for y
  
  return bod;
}

