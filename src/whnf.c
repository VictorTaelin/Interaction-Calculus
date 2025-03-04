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
uint32_t sp = 0;

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
  uint32_t stop = sp;
  Term next = term;

  while (1) {
    TermTag tag = TERM_TAG(next);

    switch (tag) {
      case VAR: {
        uint32_t var_loc = TERM_VAL(next);
        Term subst = heap[var_loc];
        if (TERM_SUB(subst)) {
          next = clear_sub(subst);
          continue;
        }
        break; // No substitution, so it's in WHNF
      }

      case CO0:
      case CO1: {
        uint32_t col_loc = TERM_VAL(next);
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

      case APP: {
        uint32_t app_loc = TERM_VAL(next);
        spush(next);
        next = heap[app_loc]; // Reduce the function part
        continue;
      }

      default: { // SUP, LAM
        if (sp == stop) {
          return next; // Stack empty, term is in WHNF
        } else {
          Term prev = spop();
          TermTag ptag = TERM_TAG(prev);
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
      while (sp > stop) {
        Term host = spop();
        TermTag htag = TERM_TAG(host);
        uint32_t hloc = TERM_VAL(host);
        switch (htag) {
          case APP: heap[hloc] = next; break;
          case CO0:
          case CO1: heap[hloc] = next; break;
        }
        next = host;
      }
      return next; // Return updated original term
    }
  }
}
