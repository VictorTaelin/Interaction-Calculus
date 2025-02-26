#include "whnf.h"
#include "memory.h"
#include <stdio.h>

// Global interaction counter
uint64_t interaction_count = 0;

// Reduce a term to weak head normal form
Term whnf(Term term) {
  // Continue reducing until no more reductions are possible
  while (1) {
    // Get term tag
    TermTag tag = TERM_TAG(term);

    // Handle variables with substitutions
    if (tag == VAR) {
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
      uint32_t col_loc = TERM_VAL(term);
      Term val = heap[col_loc];
      
      // If there's a substitution, continue reduction with substituted term
      if (TERM_SUB(val)) {
        term = val;
        continue;
      }
      
      // Otherwise, return the CO term as is
      return term;
    }
    
    // Handle application terms
    else if (tag == APP) {
      uint32_t app_loc = TERM_VAL(term);
      Term fun = heap[app_loc];
      
      // Reduce the function part to WHNF
      fun = whnf(fun);
      heap[app_loc] = fun;
      
      TermTag fun_tag = TERM_TAG(fun);
      
      // Handle APP-LAM interaction
      if (fun_tag == LAM) {
        interaction_count++;
        term = app_lam(term, fun);
        continue;
      }
      // Handle APP-SUP interaction
      else if (fun_tag == SUP) {
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
      return term;
    }
  }
}