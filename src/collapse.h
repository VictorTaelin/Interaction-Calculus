#ifndef IC_COLLAPSE_H
#define IC_COLLAPSE_H

#include "ic.h"

// Duplication
Term ic_collapse_sups(IC* ic, Term term);
Term ic_collapse_dups(IC* ic, Term term);

// -----------------------------------------------------------------------------
// Collapse Interactions
// -----------------------------------------------------------------------------

// λx.&L{f0,f1}
// ----------------- SUP-LAM
// x <- &L{x0,x1}
// &L{λx0.f0,λx1.f1}
static inline Term ic_sup_lam(IC* ic, Term lam, Term sup) {
  ic->interactions++;

  uint32_t lam_loc = TERM_VAL(lam);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  Term f0 = ic->heap[sup_loc + 0];
  Term f1 = ic->heap[sup_loc + 1];

  // Allocate two new LAM nodes
  uint32_t lam0_loc = ic_alloc(ic, 1);
  uint32_t lam1_loc = ic_alloc(ic, 1);
  ic->heap[lam0_loc + 0] = f0;
  ic->heap[lam1_loc + 0] = f1;

  // Create variables x0 and x1 pointing to lam0 and lam1
  Term x0 = ic_make_term(VAR, lam0_loc);
  Term x1 = ic_make_term(VAR, lam1_loc);

  // Create the new SUP &L{x0,x1}
  uint32_t new_sup_loc = ic_alloc(ic, 2);
  ic->heap[new_sup_loc + 0] = x0;
  ic->heap[new_sup_loc + 1] = x1;
  Term new_sup = ic_make_sup(sup_lab, new_sup_loc);

  // Set substitution for x (original LAM variable)
  ic->heap[lam_loc] = ic_make_sub(new_sup);

  // Create the result SUP &L{lam0, lam1}
  Term lam0_term = ic_make_term(LAM, lam0_loc);
  Term lam1_term = ic_make_term(LAM, lam1_loc);
  uint32_t result_sup_loc = ic_alloc(ic, 2);
  ic->heap[result_sup_loc + 0] = lam0_term;
  ic->heap[result_sup_loc + 1] = lam1_term;
  return ic_make_sup(sup_lab, result_sup_loc);
}

// (f &L{x0,x1})
// ------------------- SUP-APP
// !&L{f0,f1} = f
// &L{(f0 x0),(f1 x1)}
static inline Term ic_sup_app(IC* ic, Term app, Term sup) {
  ic->interactions++;

  uint32_t app_loc = TERM_VAL(app);
  uint8_t sup_lab = TERM_LAB(sup);
  Term fun = ic->heap[app_loc + 0];
  uint32_t sup_loc = TERM_VAL(sup);
  Term lft = ic->heap[sup_loc + 0];
  Term rgt = ic->heap[sup_loc + 1];

  // Allocate DUP node for fun
  uint32_t dup_loc = ic_alloc(ic, 1);
  ic->heap[dup_loc] = fun;

  // Create f0 and f1
  Term f0 = ic_make_co0(sup_lab, dup_loc);
  Term f1 = ic_make_co1(sup_lab, dup_loc);

  // Create app0 = (f0 lft)
  uint32_t app0_loc = ic_alloc(ic, 2);
  ic->heap[app0_loc + 0] = f0;
  ic->heap[app0_loc + 1] = lft;
  Term app0 = ic_make_term(APP, app0_loc);

  // Create app1 = (f1 rgt)
  uint32_t app1_loc = ic_alloc(ic, 2);
  ic->heap[app1_loc + 0] = f1;
  ic->heap[app1_loc + 1] = rgt;
  Term app1 = ic_make_term(APP, app1_loc);

  // Create result SUP &L{app0, app1}
  uint32_t result_sup_loc = ic_alloc(ic, 2);
  ic->heap[result_sup_loc + 0] = app0;
  ic->heap[result_sup_loc + 1] = app1;
  return ic_make_sup(sup_lab, result_sup_loc);
}

// &R{&L{x0,x1},y}
// ----------------------- SUP-SUP-X (if R>L)
// !&R{y0,y1} = y;
// &L{&R{x0,x1},&R{y0,y1}}
static inline Term ic_sup_sup_x(IC* ic, Term outer_sup, Term inner_sup) {
  ic->interactions++;

  uint32_t outer_sup_loc = TERM_VAL(outer_sup);
  uint8_t outer_lab = TERM_LAB(outer_sup);
  uint32_t inner_sup_loc = TERM_VAL(inner_sup);
  uint8_t inner_lab = TERM_LAB(inner_sup);
  Term x0 = ic->heap[inner_sup_loc + 0];
  Term x1 = ic->heap[inner_sup_loc + 1];
  Term y = ic->heap[outer_sup_loc + 1];

  // Allocate DUP node for y with label outer_lab
  uint32_t dup_loc = ic_alloc(ic, 1);
  ic->heap[dup_loc] = y;

  // Create y0 and y1 with label outer_lab
  Term y0 = ic_make_co0(outer_lab, dup_loc);
  Term y1 = ic_make_co1(outer_lab, dup_loc);

  // Create sup0 = &outer_lab{x0, y0}
  uint32_t sup0_loc = ic_alloc(ic, 2);
  ic->heap[sup0_loc + 0] = x0;
  ic->heap[sup0_loc + 1] = y0;
  Term sup0 = ic_make_sup(outer_lab, sup0_loc);

  // Create sup1 = &outer_lab{x1, y1}
  uint32_t sup1_loc = ic_alloc(ic, 2);
  ic->heap[sup1_loc + 0] = x1;
  ic->heap[sup1_loc + 1] = y1;
  Term sup1 = ic_make_sup(outer_lab, sup1_loc);

  // Create result SUP &inner_lab{sup0, sup1}
  uint32_t result_sup_loc = ic_alloc(ic, 2);
  ic->heap[result_sup_loc + 0] = sup0;
  ic->heap[result_sup_loc + 1] = sup1;
  return ic_make_sup(inner_lab, result_sup_loc);
}

// &R{x,&L{y0,y1}}
// ----------------------- SUP-SUP-Y (if R>L)
// !&R{x0,x1} = x;
// &L{&R{x0,x1},&R{y0,y1}}
static inline Term ic_sup_sup_y(IC* ic, Term outer_sup, Term inner_sup) {
  ic->interactions++;

  uint32_t outer_sup_loc = TERM_VAL(outer_sup);
  uint8_t outer_lab = TERM_LAB(outer_sup);
  uint32_t inner_sup_loc = TERM_VAL(inner_sup);
  uint8_t inner_lab = TERM_LAB(inner_sup);
  Term x = ic->heap[outer_sup_loc + 0];
  Term y0 = ic->heap[inner_sup_loc + 0];
  Term y1 = ic->heap[inner_sup_loc + 1];

  // Allocate DUP node for x with label outer_lab
  uint32_t dup_loc = ic_alloc(ic, 1);
  ic->heap[dup_loc] = x;

  // Create x0 and x1 with label outer_lab
  Term x0 = ic_make_co0(outer_lab, dup_loc);
  Term x1 = ic_make_co1(outer_lab, dup_loc);

  // Create sup0 = &outer_lab{x0, y0}
  uint32_t sup0_loc = ic_alloc(ic, 2);
  ic->heap[sup0_loc + 0] = x0;
  ic->heap[sup0_loc + 1] = y0;
  Term sup0 = ic_make_sup(outer_lab, sup0_loc);

  // Create sup1 = &outer_lab{x1, y1}
  uint32_t sup1_loc = ic_alloc(ic, 2);
  ic->heap[sup1_loc + 0] = x1;
  ic->heap[sup1_loc + 1] = y1;
  Term sup1 = ic_make_sup(outer_lab, sup1_loc);

  // Create result SUP &inner_lab{sup0, sup1}
  uint32_t result_sup_loc = ic_alloc(ic, 2);
  ic->heap[result_sup_loc + 0] = sup0;
  ic->heap[result_sup_loc + 1] = sup1;
  return ic_make_sup(inner_lab, result_sup_loc);
}

// !&L{x0,x1} = x; K
// ----------------- DUP-VAR
// x0 <- x
// x1 <- x
// K
static inline Term ic_dup_var(IC* ic, Term dup, Term var) {
  ic->interactions++;
  uint32_t dup_loc = TERM_VAL(dup);
  ic->heap[dup_loc] = ic_make_sub(var);
  return var;
}

// !&L{a0,a1} = (f x); K
// --------------------- DUP-APP
// a0 <- (f0 x0)
// a1 <- (f1 x1)
// !&L{f0,f1} = f;
// !&L{x0,x1} = x;
// K
static inline Term ic_dup_app(IC* ic, Term dup, Term app) {
  ic->interactions++;

  uint32_t dup_loc = TERM_VAL(dup);
  uint8_t lab = TERM_LAB(dup);
  TermTag tag = TERM_TAG(dup);
  bool is_co0 = IS_CO0(tag);

  uint32_t app_loc = TERM_VAL(app);
  Term fun = ic->heap[app_loc + 0];
  Term arg = ic->heap[app_loc + 1];

  // Allocate DUP nodes for fun and arg
  uint32_t dup_fun_loc = ic_alloc(ic, 1);
  ic->heap[dup_fun_loc] = fun;
  uint32_t dup_arg_loc = ic_alloc(ic, 1);
  ic->heap[dup_arg_loc] = arg;

  // Create CO0 and CO1 for fun
  Term f0 = ic_make_co0(lab, dup_fun_loc);
  Term f1 = ic_make_co1(lab, dup_fun_loc);

  // Create CO0 and CO1 for arg
  Term x0 = ic_make_co0(lab, dup_arg_loc);
  Term x1 = ic_make_co1(lab, dup_arg_loc);

  // Create app0 = (f0 x0)
  uint32_t app0_loc = ic_alloc(ic, 2);
  ic->heap[app0_loc + 0] = f0;
  ic->heap[app0_loc + 1] = x0;
  Term app0 = ic_make_term(APP, app0_loc);

  // Create app1 = (f1 x1)
  uint32_t app1_loc = ic_alloc(ic, 2);
  ic->heap[app1_loc + 0] = f1;
  ic->heap[app1_loc + 1] = x1;
  Term app1 = ic_make_term(APP, app1_loc);

  // Set substitution and return
  if (is_co0) {
    ic->heap[dup_loc] = ic_make_sub(app1);
    return app0;
  } else {
    ic->heap[dup_loc] = ic_make_sub(app0);
    return app1;
  }
}

#endif // IC_COLLAPSE_H
