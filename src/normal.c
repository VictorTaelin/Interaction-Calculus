#include "normal.h"
#include "whnf.h"
#include "memory.h"

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