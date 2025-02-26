//./types.h//
//./whnf.h//
//./memory.h//
//./show.h//

#include "memory.h"
#include "show.h"
#include "whnf.h"
#include <stdio.h>

// Global interaction counter
uint64_t interaction_count = 0;

// Maximum number of reductions to prevent infinite loops
#define MAX_REDUCTIONS 20

static uint32_t reduction_count = 0;

// Reduce a term to weak head normal form
Term whnf(Term term) {
  printf("whnf ");
  show_term(stdout, term);
  printf("\n");

  // Continue reducing until no more reductions are possible or max count reached
  while (reduction_count < MAX_REDUCTIONS) {
    reduction_count++;
    // Get term tag
    TermTag tag = TERM_TAG(term);

    // Handle variables with substitutions
    if (tag == VAR) {
      printf("var\n");
      uint32_t var_loc = TERM_VAL(term);
      Term subst = heap[var_loc];
      
      // If there's a substitution, continue reduction with substituted term
      if (TERM_SUB(subst)) {
        term = subst;
        continue;
      } else {
        // No substitution, term is in WHNF
        return term;
      }
    }
    
    // Handle collapse variables (CO0, CO1)
    else if (tag == CO0 || tag == CO1) {
      printf("col\n");
      uint32_t col_loc = TERM_VAL(term);
      Term val = heap[col_loc];
      
      // If there's a substitution, continue reduction with substituted term
      if (TERM_SUB(val)) {
        term = val;
        continue;
      }
      
      // If not a substitution, reduce the value at collapse location
      val = whnf(val);
      heap[col_loc] = val; // Store the reduced value back
      
      TermTag val_tag = TERM_TAG(val);
      
      // Apply appropriate interactions based on the value's tag
      if (val_tag == LAM) {
        interaction_count++;
        term = col_lam(term, val);
        continue;
      }
      else if (val_tag == SUP) {
        interaction_count++;
        term = col_sup(term, val);
        continue;
      }
      else if (val_tag == NIL) {
        interaction_count++;
        term = col_nil(term, val);
        continue;
      }
      else if (val_tag == B_0) {
        interaction_count++;
        term = col_b_0(term, val);
        continue;
      }
      else if (val_tag == B_1) {
        interaction_count++;
        term = col_b_1(term, val);
        continue;
      }
      else if (val_tag == TUP) {
        interaction_count++;
        term = col_tup(term, val);
        continue;
      }
      else {
        // No interaction available, term is in WHNF
        return term;
      }
    }
    
    // Handle application terms
    else if (tag == APP) {
      printf("app\n");
      uint32_t app_loc = TERM_VAL(term);
      Term fun = heap[app_loc];
      
      // Reduce the function part to WHNF
      fun = whnf(fun);
      heap[app_loc] = fun;
      
      TermTag fun_tag = TERM_TAG(fun);
      
      // Handle APP-LAM interaction
      if (fun_tag == LAM) {
        printf("app_lam\n");
        interaction_count++;
        term = app_lam(term, fun);
        continue;
      }
      // Handle APP-SUP interaction
      else if (fun_tag == SUP) {
        printf("app_sup\n");
        interaction_count++;
        term = app_sup(term, fun);
        continue;
      }
      else {
        // No reduction, term is in WHNF
        return term;
      }
    }
    
    // Handle use/elimination terms
    else if (tag == USE) {
      printf("use\n");
      uint32_t use_loc = TERM_VAL(term);
      Term val = heap[use_loc];
      
      // Reduce the value to WHNF
      val = whnf(val);
      heap[use_loc] = val;
      
      TermTag val_tag = TERM_TAG(val);
      
      // Handle USE-NIL interaction
      if (val_tag == NIL) {
        interaction_count++;
        term = use_nil(term, val);
        continue;
      }
      // Handle USE-SUP interaction
      else if (val_tag == SUP) {
        interaction_count++;
        term = use_sup(term, val);
        continue;
      }
      else {
        // No reduction, term is in WHNF
        return term;
      }
    }
    
    // Handle if-then-else terms
    else if (tag == ITE) {
      printf("ite\n");
      uint32_t ite_loc = TERM_VAL(term);
      Term cond = heap[ite_loc];
      
      // Reduce the condition to WHNF
      cond = whnf(cond);
      heap[ite_loc] = cond;
      
      TermTag cond_tag = TERM_TAG(cond);
      
      // Handle ITE-B_0 interaction
      if (cond_tag == B_0) {
        interaction_count++;
        term = ite_b_0(term, cond);
        continue;
      }
      // Handle ITE-B_1 interaction
      else if (cond_tag == B_1) {
        interaction_count++;
        term = ite_b_1(term, cond);
        continue;
      }
      // Handle ITE-SUP interaction
      else if (cond_tag == SUP) {
        interaction_count++;
        term = ite_sup(term, cond);
        continue;
      }
      else {
        // No reduction, term is in WHNF
        return term;
      }
    }
    
    // Handle get/projection terms
    else if (tag == GET) {
      printf("get\n");
      uint32_t get_loc = TERM_VAL(term);
      Term val = heap[get_loc + 2]; // The pair value is at index 2
      
      // Reduce the value to WHNF
      val = whnf(val);
      heap[get_loc + 2] = val;
      
      TermTag val_tag = TERM_TAG(val);
      
      // Handle GET-TUP interaction
      if (val_tag == TUP) {
        interaction_count++;
        term = get_tup(term, val);
        continue;
      }
      // Handle GET-SUP interaction
      else if (val_tag == SUP) {
        interaction_count++;
        term = get_sup(term, val);
        continue;
      }
      else {
        // No reduction, term is in WHNF
        return term;
      }
    }
    
    // Handle rewrite terms
    else if (tag == RWT) {
      printf("rwt\n");
      uint32_t rwt_loc = TERM_VAL(term);
      Term val = heap[rwt_loc];
      
      // Reduce the value to WHNF
      val = whnf(val);
      heap[rwt_loc] = val;
      
      TermTag val_tag = TERM_TAG(val);
      
      // Handle RWT-RFL interaction
      if (val_tag == RFL) {
        interaction_count++;
        term = rwt_rfl(term, val);
        continue;
      }
      // Handle RWT-SUP interaction
      else if (val_tag == SUP) {
        interaction_count++;
        term = rwt_sup(term, val);
        continue;
      }
      else {
        // No reduction, term is in WHNF
        return term;
      }
    }
    
    // For all other terms, they are already in WHNF
    else {
      printf("done\n");
      return term;
    }
  }
  
  // If we've reached the maximum number of reductions, we likely have an infinite loop
  fprintf(stderr, "Error: Maximum reduction count reached, possible infinite loop\n");
  return term;
}
