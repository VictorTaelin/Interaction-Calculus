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

// Reduce a term to weak head normal form
Term whnf(Term term) {
  while (1) {
    //printf("%d\n", interaction_count);
    //if (interaction_count >= 1) return term;
    //TermTag tag = TERM_TAG(term);

    switch (tag) {
      case VAR: {
        uint32_t var_loc = TERM_VAL(term);
        Term subst = heap[var_loc];
        if (TERM_SUB(subst)) {
          term = clear_sub(subst);
          continue;
        }
        return term;
      }

      case LET: {
        term = let_red(term);
        continue;
      }

      case CO0:
      case CO1: {
        uint32_t col_loc = TERM_VAL(term);
        Term val = heap[col_loc];
        if (TERM_SUB(val)) {
          term = clear_sub(val);
          continue;
        }
        val = whnf(val);
        heap[col_loc] = val;
        switch (TERM_TAG(val)) {
          case LAM: term = col_lam(term, val); continue;
          case SUP: term = col_sup(term, val); continue;
          case NIL: term = col_nil(term, val); continue;
          case BT0: term = col_b_0(term, val); continue;
          case BT1: term = col_b_1(term, val); continue;
          case TUP: term = col_tup(term, val); continue;
          default: return term;
        }
      }

      case APP: {
        uint32_t app_loc = TERM_VAL(term);
        Term fun = whnf(heap[app_loc]);
        heap[app_loc] = fun;
        switch (TERM_TAG(fun)) {
          case LAM: term = app_lam(term, fun); continue;
          case SUP: term = app_sup(term, fun); continue;
          default: return term;
        }
      }

      case USE: {
        uint32_t use_loc = TERM_VAL(term);
        Term val = whnf(heap[use_loc]);
        heap[use_loc] = val;
        switch (TERM_TAG(val)) {
          case NIL: term = use_nil(term, val); continue;
          case SUP: term = use_sup(term, val); continue;
          default: return term;
        }
      }

      case ITE: {
        uint32_t ite_loc = TERM_VAL(term);
        Term cond = whnf(heap[ite_loc]);
        heap[ite_loc] = cond;
        switch (TERM_TAG(cond)) {
          case BT0: term = ite_b_0(term, cond); continue;
          case BT1: term = ite_b_1(term, cond); continue;
          case SUP: term = ite_sup(term, cond); continue;
          default: return term;
        }
      }

      case GET: {
        uint32_t get_loc = TERM_VAL(term);
        Term val = whnf(heap[get_loc + 2]);
        heap[get_loc + 2] = val;
        switch (TERM_TAG(val)) {
          case TUP: term = get_tup(term, val); continue;
          case SUP: term = get_sup(term, val); continue;
          default: return term;
        }
      }

      case RWT: {
        uint32_t rwt_loc = TERM_VAL(term);
        Term val = whnf(heap[rwt_loc]);
        heap[rwt_loc] = val;
        switch (TERM_TAG(val)) {
          case RFL: term = rwt_rfl(term, val); continue;
          case SUP: term = rwt_sup(term, val); continue;
          default: return term;
        }
      }

      default:
        return term;
    }
  }

  return term;
}
