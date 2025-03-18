#include "ic.h"

// -----------------------------------------------------------------------------
// Memory Management Functions
// -----------------------------------------------------------------------------

// Create a new IC context with the specified heap and stack sizes.
// @param heap_size Number of terms in the heap
// @param stack_size Number of terms in the stack
// @return A new IC context or NULL if allocation failed
inline IC* ic_new(Val heap_size, Val stack_size) {
  IC* ic = (IC*)malloc(sizeof(IC));
  if (!ic) return NULL;

  // Initialize structure
  ic->heap_size = heap_size;
  ic->stack_size = stack_size;
  ic->heap_pos = 0;
  ic->interactions = 0;
  ic->stack_pos = 0;

  // Allocate heap and stack
  ic->heap = (Term*)calloc(heap_size, sizeof(Term));
  ic->stack = (Term*)malloc(stack_size * sizeof(Term));

  if (!ic->heap || !ic->stack) {
    ic_free(ic);
    return NULL;
  }

  return ic;
}

// Create a new IC context with default heap and stack sizes.
// @return A new IC context or NULL if allocation failed
inline IC* ic_default_new() {
  return ic_new(IC_DEFAULT_HEAP_SIZE, IC_DEFAULT_STACK_SIZE);
}

// Free all resources associated with an IC context.
// @param ic The IC context to free
inline void ic_free(IC* ic) {
  if (!ic) return;

  if (ic->heap) free(ic->heap);
  if (ic->stack) free(ic->stack);

  free(ic);
}

// Allocate n consecutive terms in memory.
// @param ic The IC context
// @param n Number of terms to allocate
// @return Location in the heap
// Does NOT bound check. We'll add a less frequent checker elsewhere.
inline Val ic_alloc(IC* ic, Val n) {
  Val ptr = ic->heap_pos;
  ic->heap_pos += n;
  return ptr;
}

// -----------------------------------------------------------------------------
// Term Manipulation Functions
// -----------------------------------------------------------------------------

// Create a term with the given tag and value.
// @param tag Term type tag (includes label for SUP, CX, CY)
// @param val Value/pointer into the heap
// @return The constructed term
inline Term ic_make_term(TermTag tag, Val val) {
  return MAKE_TERM(false, tag, val);
}

// Create a substitution term.
// @param term The term to convert to a substitution
// @return The term with its substitution bit set
inline Term ic_make_sub(Term term) {
  return term | TERM_SUB_MASK;
}

// Remove the substitution bit from a term.
// @param term The term to clear the substitution bit from
// @return The term with its substitution bit cleared
inline Term ic_clear_sub(Term term) {
  return term & ~TERM_SUB_MASK;
}

// Helper to create a term with the appropriate superposition tag for a label
// @param lab Label value (0-3)
// @param val Value/pointer into the heap
// @return The constructed superposition term
inline Term ic_make_sup(Lab lab, Val val) {
  return ic_make_term(SUP_TAG(lab), val);
}

// Helper to create a DP0 term with the appropriate tag for a label
// @param lab Label value (0-3)
// @param val Value/pointer into the heap
// @return The constructed DP0 term
inline Term ic_make_co0(Lab lab, Val val) {
  return ic_make_term(DP0_TAG(lab), val);
}

// Helper to create a DP1 term with the appropriate tag for a label
// @param lab Label value (0-3)
// @param val Value/pointer into the heap
// @return The constructed DP1 term
inline Term ic_make_co1(Lab lab, Val val) {
  return ic_make_term(DP1_TAG(lab), val);
}

// Helper to create an erasure term
// @return An erasure term (ERA tag with no value)
inline Term ic_make_era() {
  return ic_make_term(ERA, 0);
}

// Helper to create a number term
// @param val The numeric value
// @return A number term
inline Term ic_make_num(Val val) {
  return ic_make_term(NUM, val);
}

// Helper to create a successor term
// @param val Pointer to the successor node
// @return A successor term
inline Term ic_make_suc(Val val) {
  return ic_make_term(SUC, val);
}

// Helper to create a switch term
// @param val Pointer to the switch node
// @return A switch term
inline Term ic_make_swi(Val val) {
  return ic_make_term(SWI, val);
}

// Check if a term is an erasure
// @param term The term to check
// @return True if the term is an erasure, false otherwise
inline bool ic_is_era(Term term) {
  return TERM_TAG(term) == ERA;
}

// Allocs a Lam node
inline Val ic_lam(IC* ic, Term bod) {
  Val lam_loc = ic_alloc(ic, 1);
  ic->heap[lam_loc + 0] = bod;
  return lam_loc;
}

// Allocs an App node
inline Val ic_app(IC* ic, Term fun, Term arg) {
  Val app_loc = ic_alloc(ic, 2);
  ic->heap[app_loc + 0] = fun;
  ic->heap[app_loc + 1] = arg;
  return app_loc;
}

// Allocs a Sup node
inline Val ic_sup(IC* ic, Term lft, Term rgt) {
  Val sup_loc = ic_alloc(ic, 2);
  ic->heap[sup_loc + 0] = lft;
  ic->heap[sup_loc + 1] = rgt;
  return sup_loc;
}

// Allocs a Dup node
inline Val ic_dup(IC* ic, Term val) {
  Val dup_loc = ic_alloc(ic, 1);
  ic->heap[dup_loc] = val;
  return dup_loc;
}

// Allocs a Suc node
inline Val ic_suc(IC* ic, Term num) {
  Val suc_loc = ic_alloc(ic, 1);
  ic->heap[suc_loc] = num;
  return suc_loc;
}

// Allocs a Swi node
inline Val ic_swi(IC* ic, Term num, Term ifz, Term ifs) {
  Val swi_loc = ic_alloc(ic, 3);
  ic->heap[swi_loc + 0] = num;
  ic->heap[swi_loc + 1] = ifz;
  ic->heap[swi_loc + 2] = ifs;
  return swi_loc;
}

// -----------------------------------------------------------------------------
// Core Interactions
// -----------------------------------------------------------------------------

//(λx.f a)
//-------- APP-LAM
//x <- a
//f
inline Term ic_app_lam(IC* ic, Term app, Term lam) {
  ic->interactions++;

  Val app_loc = TERM_VAL(app);
  Val lam_loc = TERM_VAL(lam);

  Term arg = ic->heap[app_loc + 1];
  Term bod = ic->heap[lam_loc + 0];

  // Create substitution for the lambda variable
  ic->heap[lam_loc] = ic_make_sub(arg);

  return bod;
}

//(* a)
//----- APP-ERA
//*
inline Term ic_app_era(IC* ic, Term app, Term era) {
  ic->interactions++;
  return era; // Return the erasure term
}

//(&L{a,b} c)
//----------------- APP-SUP
//! &L{c0,c1} = c;
//&L{(a c0),(b c1)}
inline Term ic_app_sup(IC* ic, Term app, Term sup) {
  ic->interactions++;

  Val app_loc = TERM_VAL(app);
  Val sup_loc = TERM_VAL(sup);
  Lab sup_lab = TERM_LAB(sup);
  TermTag sup_tag = TERM_TAG(sup);

  Term arg = ic->heap[app_loc + 1];
  Term lft = ic->heap[sup_loc + 0];
  Term rgt = ic->heap[sup_loc + 1];

  // Allocate only what's necessary
  Val dup_loc = ic_alloc(ic, 1);
  Val app1_loc = ic_alloc(ic, 2);

  // Store the arg in the duplication location
  ic->heap[dup_loc] = arg;

  // Create DP0 and DP1 terms
  Term x0 = ic_make_co0(sup_lab, dup_loc);
  Term x1 = ic_make_co1(sup_lab, dup_loc);

  // Reuse sup_loc for app0
  ic->heap[sup_loc + 1] = x0; // lft is already in heap[sup_loc + 0]

  // Set up app1
  ic->heap[app1_loc + 0] = rgt;
  ic->heap[app1_loc + 1] = x1;

  // Reuse app_loc for the result superposition
  ic->heap[app_loc + 0] = ic_make_term(APP, sup_loc);
  ic->heap[app_loc + 1] = ic_make_term(APP, app1_loc);

  // Use same superposition tag as input
  return ic_make_term(sup_tag, app_loc);
}

//! &L{r,s} = *;
//K
//-------------- DUP-ERA
//r <- *
//s <- *
//K
inline Term ic_dup_era(IC* ic, Term dup, Term era) {
  ic->interactions++;

  Val dup_loc = TERM_VAL(dup);
  TermTag dup_tag = TERM_TAG(dup);
  bool is_co0 = IS_DP0(dup_tag);

  // Create erasure term for substitution
  Term era_term = ic_make_era();

  // Set substitution
  ic->heap[dup_loc] = ic_make_sub(era_term);

  // Return an erasure
  return era_term;
}

//! &L{r,s} = λx.f;
//K
//----------------- DUP-LAM
//r <- λx0.f0
//s <- λx1.f1
//x <- &L{x0,x1}
//! &L{f0,f1} = f;
//K
inline Term ic_dup_lam(IC* ic, Term dup, Term lam) {
  ic->interactions++;

  Val dup_loc = TERM_VAL(dup);
  Val lam_loc = TERM_VAL(lam);
  Lab dup_lab = TERM_LAB(dup);
  TermTag dup_tag = TERM_TAG(dup);
  bool is_co0 = IS_DP0(dup_tag);

  Term bod = ic->heap[lam_loc + 0];

  // Batch allocate memory for efficiency
  Val alloc_start = ic_alloc(ic, 5);
  Val lam0_loc = alloc_start;
  Val lam1_loc = alloc_start + 1;
  Val sup_loc = alloc_start + 2; // 2 locations
  Val dup_new_loc = alloc_start + 4;

  // Set up the superposition
  ic->heap[sup_loc + 0] = ic_make_term(VAR, lam0_loc);
  ic->heap[sup_loc + 1] = ic_make_term(VAR, lam1_loc);

  // Replace lambda's variable with the superposition
  ic->heap[lam_loc] = ic_make_sub(ic_make_sup(dup_lab, sup_loc));

  // Set up the new duplication
  ic->heap[dup_new_loc] = bod;

  // Set up new lambda bodies
  ic->heap[lam0_loc] = ic_make_co0(dup_lab, dup_new_loc);
  ic->heap[lam1_loc] = ic_make_co1(dup_lab, dup_new_loc);

  // Create and return the appropriate lambda
  if (is_co0) {
    ic->heap[dup_loc] = ic_make_sub(ic_make_term(LAM, lam1_loc));
    return ic_make_term(LAM, lam0_loc);
  } else {
    ic->heap[dup_loc] = ic_make_sub(ic_make_term(LAM, lam0_loc));
    return ic_make_term(LAM, lam1_loc);
  }
}

//! &L{x,y} = &L{a,b};
//K
//-------------------- DUP-SUP (if equal labels)
//x <- a
//y <- b
//K

//! &L{x,y} = &R{a,b};
//K
//-------------------- DUP-SUP (if different labels)
//x <- &R{a0,b0} 
//y <- &R{a1,b1}
//! &L{a0,a1} = a
//! &L{b0,b1} = b
//K
inline Term ic_dup_sup(IC* ic, Term dup, Term sup) {
  ic->interactions++;

  Val dup_loc = TERM_VAL(dup);
  Val sup_loc = TERM_VAL(sup);
  Lab dup_lab = TERM_LAB(dup);
  Lab sup_lab = TERM_LAB(sup);
  TermTag dup_tag = TERM_TAG(dup);
  TermTag sup_tag = TERM_TAG(sup);
  bool is_co0 = IS_DP0(dup_tag);

  Term lft = ic->heap[sup_loc + 0];
  Term rgt = ic->heap[sup_loc + 1];

  // Fast path for matching labels (common case)
  if (dup_lab == sup_lab) {
    // Labels match: simple substitution
    if (is_co0) {
      ic->heap[dup_loc] = ic_make_sub(rgt);
      return lft;
    } else {
      ic->heap[dup_loc] = ic_make_sub(lft);
      return rgt;
    }
  } else {
    // Labels don't match: create nested duplications
    Val sup_start = ic_alloc(ic, 4); // 2 sups with 2 terms each
    Val sup0_loc = sup_start;
    Val sup1_loc = sup_start + 2;

    // Use existing locations as duplication locations
    Val dup_lft_loc = sup_loc + 0;
    Val dup_rgt_loc = sup_loc + 1;

    // Set up the first superposition (for DP0)
    ic->heap[sup0_loc + 0] = ic_make_co0(dup_lab, dup_lft_loc);
    ic->heap[sup0_loc + 1] = ic_make_co0(dup_lab, dup_rgt_loc);

    // Set up the second superposition (for DP1)
    ic->heap[sup1_loc + 0] = ic_make_co1(dup_lab, dup_lft_loc);
    ic->heap[sup1_loc + 1] = ic_make_co1(dup_lab, dup_rgt_loc);

    // Set up original duplications to point to lft and rgt
    ic->heap[dup_lft_loc] = lft;
    ic->heap[dup_rgt_loc] = rgt;

    if (is_co0) {
      ic->heap[dup_loc] = ic_make_sub(ic_make_sup(sup_lab, sup1_loc));
      return ic_make_sup(sup_lab, sup0_loc);
    } else {
      ic->heap[dup_loc] = ic_make_sub(ic_make_sup(sup_lab, sup0_loc));
      return ic_make_sup(sup_lab, sup1_loc);
    }
  }
}

// -----------------------------------------------------------------------------
// Numeric Interactions
// -----------------------------------------------------------------------------

//+N
//--- SUC-NUM
//N+1
inline Term ic_suc_num(IC* ic, Term suc, Term num) {
  ic->interactions++;
  uint32_t num_val = TERM_VAL(num);
  return ic_make_num(num_val + 1);
}

//+*
//-- SUC-ERA
//*
inline Term ic_suc_era(IC* ic, Term suc, Term era) {
  ic->interactions++;
  return era; // Erasure propagates
}

//+&L{x,y}
//--------- SUC-SUP
//&L{+x,+y}
inline Term ic_suc_sup(IC* ic, Term suc, Term sup) {
  ic->interactions++;

  Val sup_loc = TERM_VAL(sup);
  Lab sup_lab = TERM_LAB(sup);

  Term lft = ic->heap[sup_loc + 0];
  Term rgt = ic->heap[sup_loc + 1];

  // Create SUC nodes for each branch
  Val suc0_loc = ic_suc(ic, lft);
  Val suc1_loc = ic_suc(ic, rgt);

  // Create the resulting superposition of SUCs
  Val res_loc = ic_alloc(ic, 2);
  ic->heap[res_loc + 0] = ic_make_suc(suc0_loc);
  ic->heap[res_loc + 1] = ic_make_suc(suc1_loc);

  return ic_make_sup(sup_lab, res_loc);
}

//?N{0:z;+:s;}
//------------ SWI-NUM (if N==0)
//z
inline Term ic_swi_num(IC* ic, Term swi, Term num) {
  ic->interactions++;

  Val swi_loc = TERM_VAL(swi);
  Val num_val = TERM_VAL(num);

  Term ifz = ic->heap[swi_loc + 1];
  Term ifs = ic->heap[swi_loc + 2];

  if (num_val == 0) {
    // If the number is 0, return the zero branch
    return ifz;
  } else {
    // Otherwise, apply the successor branch to N-1
    Val app_loc = ic_alloc(ic, 2);
    ic->heap[app_loc + 0] = ifs;
    ic->heap[app_loc + 1] = ic_make_num(num_val - 1);
    return ic_make_term(APP, app_loc);
  }
}

//?*{0:z;+:s;}
//------------ SWI-ERA
//*
inline Term ic_swi_era(IC* ic, Term swi, Term era) {
  ic->interactions++;
  return era; // Erasure propagates
}

//?&L{x,y}{0:z;+:s;}
//--------------------------------- SWI-SUP
//!&L{z0,z1} = z;
//!&L{s0,s1} = s;
//&L{?x{0:z0;+:s0;},?y{0:z1;+:s1;}}
inline Term ic_swi_sup(IC* ic, Term swi, Term sup) {
  ic->interactions++;

  Val swi_loc = TERM_VAL(swi);
  Val sup_loc = TERM_VAL(sup);
  Lab sup_lab = TERM_LAB(sup);

  Term lft = ic->heap[sup_loc + 0];
  Term rgt = ic->heap[sup_loc + 1];
  Term ifz = ic->heap[swi_loc + 1];
  Term ifs = ic->heap[swi_loc + 2];

  // Create duplications for ifz and ifs branches
  Val dup_z_loc = ic_alloc(ic, 1);
  Val dup_s_loc = ic_alloc(ic, 1);

  ic->heap[dup_z_loc] = ifz;
  ic->heap[dup_s_loc] = ifs;

  Term z0 = ic_make_co0(sup_lab, dup_z_loc);
  Term z1 = ic_make_co1(sup_lab, dup_z_loc);
  Term s0 = ic_make_co0(sup_lab, dup_s_loc);
  Term s1 = ic_make_co1(sup_lab, dup_s_loc);

  // Create switch nodes for each branch
  Val swi0_loc = ic_swi(ic, lft, z0, s0);
  Val swi1_loc = ic_swi(ic, rgt, z1, s1);

  // Create the resulting superposition
  Val res_loc = ic_alloc(ic, 2);
  ic->heap[res_loc + 0] = ic_make_term(SWI, swi0_loc);
  ic->heap[res_loc + 1] = ic_make_term(SWI, swi1_loc);

  return ic_make_sup(sup_lab, res_loc);
}

//! &L{x,y} = N;
//K
//-------------- DUP-NUM
//x <- N
//y <- N
//K
inline Term ic_dup_num(IC* ic, Term dup, Term num) {
  ic->interactions++;

  Val dup_loc = TERM_VAL(dup);
  Val num_val = TERM_VAL(num);
  TermTag dup_tag = TERM_TAG(dup);
  bool is_co0 = IS_DP0(dup_tag);

  // Numbers are duplicated by simply substituting both variables with the same number
  ic->heap[dup_loc] = ic_make_sub(num); // Set substitution for the other variable

  return num; // Return the number
}

// -----------------------------------------------------------------------------
// Term Normalization
// -----------------------------------------------------------------------------

// Reduce a term to weak head normal form (WHNF).
// 
// @param ic The IC context
// @param term The term to reduce
// @return The term in WHNF
inline Term ic_whnf(IC* ic, Term term) {
  Val stop = ic->stack_pos;
  Term next = term;
  Term* heap = ic->heap;
  Term* stack = ic->stack;
  Val stack_pos = stop;

  TermTag tag;
  Val val_loc;
  Term val;
  Term prev;
  TermTag ptag;

  while (1) {
    tag = TERM_TAG(next);

    // On variables: substitute
    // On eliminators: move to field
    if (tag == VAR) {
      val_loc = TERM_VAL(next);
      val = heap[val_loc];
      if (TERM_SUB(val)) {
        next = ic_clear_sub(val);
        continue;
      }
    } else if (IS_DUP(tag)) {
      val_loc = TERM_VAL(next);
      val = heap[val_loc];
      if (TERM_SUB(val)) {
        next = ic_clear_sub(val);
        continue;
      } else {
        stack[stack_pos++] = next;
        next = val;
        continue;
      }
    } else if (tag == APP) {
      val_loc = TERM_VAL(next);
      stack[stack_pos++] = next;
      next = heap[val_loc]; // Reduce the function part
      continue;
    } else if (tag == SUC) {
      val_loc = TERM_VAL(next);
      stack[stack_pos++] = next;
      next = heap[val_loc]; // Reduce the inner term
      continue;
    } else if (tag == SWI) {
      val_loc = TERM_VAL(next);
      stack[stack_pos++] = next;
      next = heap[val_loc]; // Reduce the number term
      continue;
    }

    // Empty stack: term is in WHNF
    if (stack_pos == stop) {
      ic->stack_pos = stack_pos;
      return next;
    }

    // Interaction Dispatcher
    prev = stack[--stack_pos];
    ptag = TERM_TAG(prev);
    if (ptag == APP) {
      if (tag == LAM) {
        next = ic_app_lam(ic, prev, next);
        continue;
      } else if (IS_SUP(tag)) {
        next = ic_app_sup(ic, prev, next);
        continue;
      } else if (tag == ERA) {
        next = ic_app_era(ic, prev, next);
        continue;
      }
    } else if (IS_DUP(ptag)) {
      if (tag == LAM) {
        next = ic_dup_lam(ic, prev, next);
        continue;
      } else if (IS_SUP(tag)) {
        next = ic_dup_sup(ic, prev, next);
        continue;
      } else if (tag == ERA) {
        next = ic_dup_era(ic, prev, next);
        continue;
      } else if (tag == NUM) {
        next = ic_dup_num(ic, prev, next);
        continue;
      }
    } else if (ptag == SUC) {
      if (tag == NUM) {
        next = ic_suc_num(ic, prev, next);
        continue;
      } else if (IS_SUP(tag)) {
        next = ic_suc_sup(ic, prev, next);
        continue;
      } else if (tag == ERA) {
        next = ic_suc_era(ic, prev, next);
        continue;
      }
    } else if (ptag == SWI) {
      if (tag == NUM) {
        next = ic_swi_num(ic, prev, next);
        continue;
      } else if (IS_SUP(tag)) {
        next = ic_swi_sup(ic, prev, next);
        continue;
      } else if (tag == ERA) {
        next = ic_swi_era(ic, prev, next);
        continue;
      }
    }

    // No interaction: push term back to stack
    stack[stack_pos++] = prev;

    // Check if we're done
    if (stack_pos == stop) {
      ic->stack_pos = stack_pos;
      return next;
    }

    // Update parent chain
    while (stack_pos > stop) {
      prev = stack[--stack_pos];
      ptag = TERM_TAG(prev);
      val_loc = TERM_VAL(prev);
      if (ptag == APP || ptag == SWI || IS_DUP(ptag)) {
        heap[val_loc] = next;
      }
      next = prev;
    }

    ic->stack_pos = stack_pos;
    return next;
  }
}

// Recursive implementation of normal form reduction
inline Term ic_normal(IC* ic, Term term) {
  term = ic_whnf(ic, term);
  TermTag tag = TERM_TAG(term);
  Val loc = TERM_VAL(term);

  if (ic_is_era(term) || tag == NUM) {
    // ERA and NUM have no children, so just return them
    return term;
  } else if (tag == LAM) {
    ic->heap[loc] = ic_normal(ic, ic->heap[loc]);
    return term;
  } else if (tag == APP) {
    ic->heap[loc+0] = ic_normal(ic, ic->heap[loc]);
    ic->heap[loc+1] = ic_normal(ic, ic->heap[loc+1]);
    return term;
  } else if (IS_SUP(tag)) {
    ic->heap[loc+0] = ic_normal(ic, ic->heap[loc]);
    ic->heap[loc+1] = ic_normal(ic, ic->heap[loc+1]);
    return term;
  } else if (tag == SUC) {
    ic->heap[loc] = ic_normal(ic, ic->heap[loc]);
    return term;
  } else if (tag == SWI) {
    ic->heap[loc+0] = ic_normal(ic, ic->heap[loc]);
    ic->heap[loc+1] = ic_normal(ic, ic->heap[loc+1]);
    ic->heap[loc+2] = ic_normal(ic, ic->heap[loc+2]);
    return term;
  } else {
    return term;
  }
}
