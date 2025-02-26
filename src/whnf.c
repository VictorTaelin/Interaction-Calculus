#include <stdio.h>
#include "whnf.h"
#include "memory.h"

// Initialize the interaction counter
uint64_t interaction_count = 0;

// Reduce a term to weak head normal form
Term whnf(Term term) {
  while (1) {
    // Check term tag
    TermTag tag = TERM_TAG(term);
    uint32_t val = TERM_VAL(term);

    // Handle variables by following substitutions
    if (tag == VAR || tag == CO0 || tag == CO1) {
      uint32_t loc = val;
      // Check if there's a substitution at the location
      if (loc < HEAP_SIZE && TERM_SUB(heap[loc])) {
        // Follow the substitution
        term = heap[loc];
        // Clear the substitution bit
        term = MAKE_TERM(false, TERM_TAG(term), TERM_LAB(term), TERM_VAL(term));
      } else {
        // No substitution, we're done
        return term;
      }
    }
    // Handle applications
    else if (tag == APP) {
      // Get the function and argument
      Term fun = heap[val];
      Term arg = heap[val + 1];
      
      // Evaluate the function to WHNF
      fun = whnf(fun);
      
      // Store the updated function
      heap[val] = fun;
      
      // Check what kind of term the function is
      TermTag fun_tag = TERM_TAG(fun);
      
      if (fun_tag == LAM) {
        // Function is a lambda, perform beta-reduction
        interaction_count++;
        term = app_lam(term, fun);
      } else if (fun_tag == SUP) {
        // Function is a superposition, perform overlap
        interaction_count++;
        term = app_sup(term, fun);
      } else {
        // Function doesn't reduce further, return the application
        return term;
      }
    }
    // Handle collapsers
    else if (tag == CO0 || tag == CO1) {
      // Determine which collapse variable we are
      uint32_t loc = val;
      // If there's no substitution at the location, we're done
      if (loc >= HEAP_SIZE || !TERM_SUB(heap[loc])) {
        return term;
      }
      // Follow the substitution
      term = heap[loc];
      // Clear the substitution bit
      term = MAKE_TERM(false, TERM_TAG(term), TERM_LAB(term), TERM_VAL(term));
    }
    // Handle LET terms
    else if (tag == LET) {
      Term let_val = heap[val];
      Term let_bod = heap[val + 1];
      // Just continue with the body
      term = let_bod;
    }
    // Handle unit type elimination
    else if (tag == USE) {
      Term val_term = whnf(heap[val]);
      heap[val] = val_term;
      
      if (TERM_TAG(val_term) == NIL) {
        // Eliminate unit value
        interaction_count++;
        term = use_nil(term, val_term);
      } else if (TERM_TAG(val_term) == SUP) {
        // Handle superposed unit elimination
        interaction_count++;
        term = use_sup(term, val_term);
      } else {
        // Can't reduce further
        return term;
      }
    }
    // Handle bool type elimination
    else if (tag == ITE) {
      Term cond = whnf(heap[val]);
      heap[val] = cond;
      
      if (TERM_TAG(cond) == B_0) {
        // False case
        interaction_count++;
        term = ite_b0(term, cond);
      } else if (TERM_TAG(cond) == B_1) {
        // True case
        interaction_count++;
        term = ite_b1(term, cond);
      } else if (TERM_TAG(cond) == SUP) {
        // Superposed condition
        interaction_count++;
        term = ite_sup(term, cond);
      } else {
        // Can't reduce further
        return term;
      }
    }
    // Handle sigma type elimination
    else if (tag == GET) {
      Term pair = whnf(heap[val + 2]);
      heap[val + 2] = pair;
      
      if (TERM_TAG(pair) == TUP) {
        // Pair projection
        interaction_count++;
        term = get_tup(term, pair);
      } else if (TERM_TAG(pair) == SUP) {
        // Superposed pair
        interaction_count++;
        term = get_sup(term, pair);
      } else {
        // Can't reduce further
        return term;
      }
    }
    // Handle equality type elimination
    else if (tag == RWT) {
      Term eq = whnf(heap[val]);
      heap[val] = eq;
      
      if (TERM_TAG(eq) == RFL) {
        // Reflexivity
        interaction_count++;
        term = rwt_rfl(term, eq);
      } else if (TERM_TAG(eq) == SUP) {
        // Superposed equality
        interaction_count++;
        term = rwt_sup(term, eq);
      } else {
        // Can't reduce further
        return term;
      }
    }
    // For any other term, we're already in WHNF
    else {
      return term;
    }
  }
}

// Reduce a term to full normal form
Term normal(Term term) {
  // First reduce to WHNF
  term = whnf(term);
  
  // Get term details
  TermTag tag = TERM_TAG(term);
  uint32_t val = TERM_VAL(term);
  
  // Recursively normalize subterms based on the term type
  switch (tag) {
    case LAM: {
      // Normalize lambda body
      Term body = normal(heap[val]);
      heap[val] = body;
      break;
    }
    case APP: {
      // Normalize function and argument
      Term fun = normal(heap[val]);
      Term arg = normal(heap[val + 1]);
      heap[val] = fun;
      heap[val + 1] = arg;
      break;
    }
    case SUP: {
      // Normalize both sides of superposition
      Term left = normal(heap[val]);
      Term right = normal(heap[val + 1]);
      heap[val] = left;
      heap[val + 1] = right;
      break;
    }
    case LET: {
      // Normalize let binding and body
      Term binding = normal(heap[val]);
      Term body = normal(heap[val + 1]);
      heap[val] = binding;
      heap[val + 1] = body;
      break;
    }
    case EFQ: {
      // Normalize argument of empty type elimination
      Term arg = normal(heap[val]);
      heap[val] = arg;
      break;
    }
    case USE: {
      // Normalize unit elimination
      Term arg = normal(heap[val]);
      Term body = normal(heap[val + 1]);
      heap[val] = arg;
      heap[val + 1] = body;
      break;
    }
    case ITE: {
      // Normalize if-then-else
      Term cond = normal(heap[val]);
      Term then_branch = normal(heap[val + 1]);
      Term else_branch = normal(heap[val + 2]);
      heap[val] = cond;
      heap[val + 1] = then_branch;
      heap[val + 2] = else_branch;
      break;
    }
    case SIG: {
      // Normalize sigma type
      Term arg = normal(heap[val]);
      Term body = normal(heap[val + 1]);
      heap[val] = arg;
      heap[val + 1] = body;
      break;
    }
    case TUP: {
      // Normalize tuple
      Term first = normal(heap[val]);
      Term second = normal(heap[val + 1]);
      heap[val] = first;
      heap[val + 1] = second;
      break;
    }
    case GET: {
      // Normalize get operation
      Term pair = normal(heap[val + 2]);
      Term body = normal(heap[val + 3]);
      heap[val + 2] = pair;
      heap[val + 3] = body;
      break;
    }
    case ALL: {
      // Normalize pi type
      Term arg = normal(heap[val]);
      Term body = normal(heap[val + 1]);
      heap[val] = arg;
      heap[val + 1] = body;
      break;
    }
    case EQL: {
      // Normalize equality type
      Term left = normal(heap[val]);
      Term right = normal(heap[val + 1]);
      heap[val] = left;
      heap[val + 1] = right;
      break;
    }
    case RWT: {
      // Normalize rewrite
      Term eq = normal(heap[val]);
      Term body = normal(heap[val + 1]);
      heap[val] = eq;
      heap[val + 1] = body;
      break;
    }
    // For other terms (VAR, CO0, CO1, etc.), nothing to normalize
    default:
      break;
  }
  
  return term;
}

// Stub implementations for individual interactions
// These will be implemented in separate files later

Term app_lam(Term app, Term lam) {
  // Placeholder - will be implemented later
  return app;
}

Term app_sup(Term app, Term sup) {
  // Placeholder - will be implemented later
  return app;
}

Term col_sup(Term col, Term sup) {
  // Placeholder - will be implemented later
  return col;
}

Term col_lam(Term col, Term lam) {
  // Placeholder - will be implemented later
  return col;
}

Term col_nil(Term col, Term nil) {
  // Placeholder - will be implemented later
  return col;
}

Term col_b_0(Term col, Term b_0) {
  // Placeholder - will be implemented later
  return col;
}

Term col_b_1(Term col, Term b_1) {
  // Placeholder - will be implemented later
  return col;
}

Term col_tup(Term col, Term tup) {
  // Placeholder - will be implemented later
  return col;
}

Term use_nil(Term use, Term nil) {
  // Placeholder - will be implemented later
  return use;
}

Term use_sup(Term use, Term sup) {
  // Placeholder - will be implemented later
  return use;
}

Term ite_b0(Term ite, Term b_0) {
  // Placeholder - will be implemented later
  return ite;
}

Term ite_b1(Term ite, Term b_1) {
  // Placeholder - will be implemented later
  return ite;
}

Term ite_sup(Term ite, Term sup) {
  // Placeholder - will be implemented later
  return ite;
}

Term get_tup(Term get, Term tup) {
  // Placeholder - will be implemented later
  return get;
}

Term get_sup(Term get, Term sup) {
  // Placeholder - will be implemented later
  return get;
}

Term rwt_rfl(Term rwt, Term rfl) {
  // Placeholder - will be implemented later
  return rwt;
}

Term rwt_sup(Term rwt, Term sup) {
  // Placeholder - will be implemented later
  return rwt;
}
