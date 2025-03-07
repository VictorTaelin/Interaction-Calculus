//./../IC.md//
//./ic.h//
//./collapse.h//

// This is a WIP

#include "ic.h"
#include "collapse.h"
#include "show.h"

Term ic_collapse_sups(IC* ic, Term term) {
  term = ic_whnf(ic, term);
  TermTag tag = TERM_TAG(term);
  uint8_t lab = TERM_LAB(term);
  uint32_t loc = TERM_VAL(term);
  if (tag == LAM) {
    Term bod_col = ic->heap[loc+0] = ic_collapse_sups(ic, ic->heap[loc+0]);
    if (IS_SUP(TERM_TAG(bod_col))) {
      //printf(">> SUP-LAM\n");
      return ic_collapse_sups(ic, ic_sup_lam(ic, term, bod_col));
    } else {
      return term;
    }
  } else if (tag == APP) {
    Term fun_col = ic->heap[loc+0] = ic_collapse_sups(ic, ic->heap[loc+0]);
    Term arg_col = ic->heap[loc+1] = ic_collapse_sups(ic, ic->heap[loc+1]);
    if (IS_SUP(TERM_TAG(arg_col))) {
      //printf(">> SUP-APP\n");
      return ic_collapse_sups(ic, ic_sup_app(ic, term, arg_col));
    } else {
      return term;
    }
  } else if (IS_SUP(tag)) {
    Term lft_col = ic->heap[loc+0] = ic_collapse_sups(ic, ic->heap[loc+0]);
    Term rgt_col = ic->heap[loc+1] = ic_collapse_sups(ic, ic->heap[loc+1]);
    if (IS_SUP(TERM_TAG(lft_col)) && lab > TERM_LAB(lft_col)) {
      //printf(">> SUP-SUP-X\n");
      return ic_collapse_sups(ic, ic_sup_sup_x(ic, term, lft_col));
    } else if (IS_SUP(TERM_TAG(rgt_col)) && lab > TERM_LAB(rgt_col)) {
      //printf(">> SUP-SUP-Y\n");
      return ic_collapse_sups(ic, ic_sup_sup_y(ic, term, rgt_col));
    } else {
      return term;
    }
  } else {
    return term;
  }
}

Term ic_collapse_dups(IC* ic, Term term) {
  term = ic_whnf(ic, term);
  TermTag tag = TERM_TAG(term);
  uint32_t loc = TERM_VAL(term);
  if (IS_DUP(tag)) {
    // Get the value this collapser points to
    Term val = ic_collapse_dups(ic, ic->heap[loc]);
    TermTag val_tag = TERM_TAG(val);
    if (val_tag == VAR) {
      //printf(">> DUP-VAR\n");
      return ic_collapse_dups(ic, ic_dup_var(ic, term, val));
    } else if (val_tag == APP) {
      //printf(">> DUP-APP\n");
      return ic_collapse_dups(ic, ic_dup_app(ic, term, val));
    } else {
      return term;
    }
  } else if (tag == LAM) {
    ic->heap[loc+0] = ic_collapse_dups(ic, ic->heap[loc+0]);
    return term;
  } else if (tag == APP) {
    ic->heap[loc+0] = ic_collapse_dups(ic, ic->heap[loc+0]);
    ic->heap[loc+1] = ic_collapse_dups(ic, ic->heap[loc+1]);
    return term;
  } else if (IS_SUP(tag)) {
    ic->heap[loc+0] = ic_collapse_dups(ic, ic->heap[loc+0]);
    ic->heap[loc+1] = ic_collapse_dups(ic, ic->heap[loc+1]);
    return term;
  } else {
    return term;
  }
}
