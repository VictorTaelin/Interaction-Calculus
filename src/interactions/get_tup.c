#include <stdio.h>
#include "../whnf.h"
#include "../memory.h"

// Implementation of GET-TUP interaction: ![x,y]=[a,b]; t -> x<-a; y<-b; t
Term get_tup(Term get, Term tup) {
  printf("get_tup\n");
  uint32_t get_loc = TERM_VAL(get);
  uint32_t tup_loc = TERM_VAL(tup);
  
  // Get components
  Term fst = heap[tup_loc];
  Term snd = heap[tup_loc + 1];
  
  // Get variables from getter
  Term x = heap[get_loc];
  Term y = heap[get_loc + 1];
  
  // Get projection body
  Term body = heap[get_loc + 3];
  
  // Create substitutions for the variables
  uint32_t x_loc = TERM_VAL(x);
  uint32_t y_loc = TERM_VAL(y);
  
  heap[x_loc] = make_sub(fst);
  heap[y_loc] = make_sub(snd);
  
  // Return the body
  return body;
}
