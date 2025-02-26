#include <stdio.h>
#include "normal.h"
#include "whnf.h"
#include "memory.h"

// Reduce a term to full normal form
Term normal(Term term) {
  //printf("normal\n");

  // First reduce to WHNF
  term = whnf(term);
  
  // Get term details
  TermTag tag = TERM_TAG(term);
  uint32_t val = TERM_VAL(term);
  
  // Recursively normalize subterms based on the term type
  switch (tag) {
    case LAM: {
      Term body = normal(heap[val]);
      heap[val] = body;
      break;
    }
    case APP: {
      Term fun = normal(heap[val]);
      Term arg = normal(heap[val + 1]);
      heap[val + 0] = fun;
      heap[val + 1] = arg;
      break;
    }
    case SUP: {
      Term left = normal(heap[val]);
      Term right = normal(heap[val + 1]);
      heap[val + 0] = left;
      heap[val + 1] = right;
      break;
    }
    case LET: {
      Term binding = normal(heap[val]);
      Term body = normal(heap[val + 1]);
      heap[val + 0] = binding;
      heap[val + 1] = body;
      break;
    }
    case EFQ: {
      Term arg = normal(heap[val]);
      heap[val] = arg;
      break;
    }
    case USE: {
      Term arg = normal(heap[val]);
      Term body = normal(heap[val + 1]);
      heap[val + 0] = arg;
      heap[val + 1] = body;
      break;
    }
    case ITE: {
      Term cond = normal(heap[val]);
      Term then_branch = normal(heap[val + 1]);
      Term else_branch = normal(heap[val + 2]);
      heap[val + 0] = cond;
      heap[val + 1] = then_branch;
      heap[val + 2] = else_branch;
      break;
    }
    case SIG: {
      Term arg = normal(heap[val]);
      Term body = normal(heap[val + 1]);
      heap[val + 0] = arg;
      heap[val + 1] = body;
      break;
    }
    case TUP: {
      Term first = normal(heap[val]);
      Term second = normal(heap[val + 1]);
      heap[val + 0] = first;
      heap[val + 1] = second;
      break;
    }
    case GET: {
      Term pair = normal(heap[val + 2]);
      Term body = normal(heap[val + 3]);
      heap[val + 2] = pair;
      heap[val + 3] = body;
      break;
    }
    case ALL: {
      Term arg = normal(heap[val]);
      Term body = normal(heap[val + 1]);
      heap[val + 0] = arg;
      heap[val + 1] = body;
      break;
    }
    case EQL: {
      Term left = normal(heap[val]);
      Term right = normal(heap[val + 1]);
      heap[val + 0] = left;
      heap[val + 1] = right;
      break;
    }
    case RWT: {
      Term eq = normal(heap[val]);
      Term body = normal(heap[val + 1]);
      heap[val + 0] = eq;
      heap[val + 1] = body;
      break;
    }
    default:
      break;
  }
  
  return term;
}
