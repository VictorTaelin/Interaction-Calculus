//./types.h//
//./memory.h//
//./show.h//
//./whnf.h//

#include "memory.h"
#include "show.h"
#include "whnf.h"
#include <stdio.h>
#include <stdlib.h>

// Global interaction counter
uint64_t interaction_count = 0;

// Manual stack for WHNF reduction
#define STACK_SIZE (1 << 26)
Term stack[STACK_SIZE];
uint64_t sp = 0;

// Helper functions for stack operations
void spush(Term term) {
  if (sp >= STACK_SIZE) {
    fprintf(stderr, "Stack overflow in WHNF reduction\n");
    exit(1);
  }
  stack[sp++] = term;
}

Term spop() {
  if (sp == 0) {
    fprintf(stderr, "Stack underflow in WHNF reduction\n");
    exit(1);
  }
  return stack[--sp];
}

// Reduce a term to weak head normal form using a manual stack
Term whnf(Term term) {
  // Save initial stack position to track when our stack frame is empty
  uint64_t stop = sp;
  Term next = term;

  while (1) {
    TermTag tag = TERM_TAG(next);

    // Process terms based on their tag
    switch (tag) {
      // Variables: follow substitutions if present
      case VAR: {
        uint64_t var_loc = TERM_VAL(next);
        Term subst = heap[var_loc];
        if (TERM_SUB(subst)) {
          next = clear_sub(subst);
          continue;
        }
        break; // No substitution means WHNF
      }

      // Let bindings: reduce immediately
      case LET: {
        next = let_red(next);
        continue;
      }

      // Collapsers (CO0, CO1): handle substitutions or push and reduce value
      case CO0:
      case CO1: {
        uint64_t col_loc = TERM_VAL(next);
        Term val = heap[col_loc];
        if (TERM_SUB(val)) {
          next = clear_sub(val);
          continue;
        } else {
          spush(next);
          next = val;
          continue;
        }
      }

      // Application: push and reduce function part
      case APP: {
        uint64_t app_loc = TERM_VAL(next);
        spush(next);
        next = heap[app_loc];
        continue;
      }

      // Unit elimination: push and reduce value
      case USE: {
        uint64_t use_loc = TERM_VAL(next);
        spush(next);
        next = heap[use_loc];
        continue;
      }

      // If-then-else: push and reduce condition
      case ITE: {
        uint64_t ite_loc = TERM_VAL(next);
        spush(next);
        next = heap[ite_loc];
        continue;
      }

      // Sigma elimination: push and reduce value
      case GET: {
        uint64_t get_loc = TERM_VAL(next);
        spush(next);
        next = heap[get_loc + 2];
        continue;
      }

      // Equality elimination: push and reduce proof
      case RWT: {
        uint64_t rwt_loc = TERM_VAL(next);
        spush(next);
        next = heap[rwt_loc];
        continue;
      }

      // Constructors: check for interactions or return if stack empty
      default: {
        if (sp == stop) {
          return next; // Stack empty, term is in WHNF
        } else {
          Term prev = spop();
          TermTag ptag = TERM_TAG(prev);

          // Check for interactions between stack top and current term
          switch (ptag) {
            case APP: {
              switch (tag) {
                case LAM: next = app_lam(prev, next); continue;
                case SUP: next = app_sup(prev, next); continue;
              }
              break;
            }
            case CO0:
            case CO1: {
              switch (tag) {
                case LAM: next = col_lam(prev, next); continue;
                case SUP: next = col_sup(prev, next); continue;
                case NIL: next = col_nil(prev, next); continue;
                case BT0: next = col_b_0(prev, next); continue;
                case BT1: next = col_b_1(prev, next); continue;
                case TUP: next = col_tup(prev, next); continue;
              }
              break;
            }
            case USE: {
              switch (tag) {
                case NIL: next = use_nil(prev, next); continue;
                case SUP: next = use_sup(prev, next); continue;
              }
              break;
            }
            case ITE: {
              switch (tag) {
                case BT0: next = ite_b_0(prev, next); continue;
                case BT1: next = ite_b_1(prev, next); continue;
                case SUP: next = ite_sup(prev, next); continue;
              }
              break;
            }
            case GET: {
              switch (tag) {
                case TUP: next = get_tup(prev, next); continue;
                case SUP: next = get_sup(prev, next); continue;
              }
              break;
            }
            case RWT: {
              switch (tag) {
                case RFL: next = rwt_rfl(prev, next); continue;
                case SUP: next = rwt_sup(prev, next); continue;
              }
              break;
            }
          }
          // No interaction found, proceed to stack traversal
          break;
        }
      }
    }

    // After processing, check stack and update heap if needed
    if (sp == stop) {
      return next; // Stack empty, return WHNF
    } else {
      // Traverse stack to update heap with reduced subterms
      while (sp > stop) {
        Term host = spop();
        TermTag htag = TERM_TAG(host);
        uint64_t hloc = TERM_VAL(host);
        switch (htag) {
          case APP: heap[hloc] = next; break;
          case CO0:
          case CO1: heap[hloc] = next; break;
          case USE: heap[hloc] = next; break;
          case ITE: heap[hloc] = next; break;
          case GET: heap[hloc + 2] = next; break;
          case RWT: heap[hloc] = next; break;
        }
        next = host;
      }
      return next; // Return updated original term
    }
  }
}
