
#ifndef HVMN_H
#define HVMN_H

// -----------------------------------------------------------------------------
// HVM-Nano (HVMN) - Core header-only implementation
// 
// This file contains the full implementation of the HVM-Nano:
// - Term representation and bit manipulation
// - Memory management
// - Core interactions (app_lam, app_sup, col_lam, col_sup)
// - Weak Head Normal Form (WHNF) reduction
// - Full Normal Form reduction
// -----------------------------------------------------------------------------

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Default heap and stack sizes
#define HVMN_DEFAULT_HEAP_SIZE (1 << 24) // 16M terms
#define HVMN_DEFAULT_STACK_SIZE (1 << 24) // 16M terms

// -----------------------------------------------------------------------------
// Core Types and Constants
// -----------------------------------------------------------------------------

// Term tags
typedef enum {
  VAR, // Variable
  CO0, // Collapser first variable
  CO1, // Collapser second variable
  APP, // Application
  CAL, // Function call
  LAM, // Lambda
  NAT, // Natural number (NUM or SUC)
  SUP, // Superposition
} TermTag;

// Term 32-bit packed representation
typedef uint32_t Term;

// Term components
#define TERM_SUB_MASK 0x80000000UL // 1-bit: Is this a substitution?
#define TERM_TAG_MASK 0x70000000UL // 3-bits: Term tag
#define TERM_LAB_MASK 0x0F000000UL // 4-bits: Label for superpositions/functions
#define SUC 0xF                    // Special label for successor (SUC) nodes
#define TERM_VAL_MASK 0x00FFFFFFUL // 24-bits: Value/pointer

// Term component extraction
#define TERM_SUB(term) (((term) & TERM_SUB_MASK) != 0)
#define TERM_TAG(term) ((TermTag)(((term) & TERM_TAG_MASK) >> 28))
#define TERM_LAB(term) (((term) & TERM_LAB_MASK) >> 24)
#define TERM_VAL(term) ((term) & TERM_VAL_MASK)

// Term creation
#define MAKE_TERM(sub, tag, lab, val) \
  (((sub) ? TERM_SUB_MASK : 0) | \
   (((uint32_t)(tag) << 28)) | \
   (((uint32_t)(lab) << 24)) | \
   ((uint32_t)(val) & TERM_VAL_MASK))

// -----------------------------------------------------------------------------
// Function Book Structure
// -----------------------------------------------------------------------------

// Maximum number of clauses per function
#define MAX_CLAUSES 16

// Maximum number of functions in the book
#define MAX_FUNCTIONS 16

// Special value used to represent the pattern-bound variable
#define PATTERN_VAR_MASK 0x00FFFFFFUL

// Represents a single clause in a function
typedef struct {
  Term* terms;         // Array of terms in the clause body
  uint32_t term_count; // Number of terms in the clause
} Clause;

// Represents a global function with pattern-matching clauses
typedef struct {
  Clause clauses[MAX_CLAUSES]; // Array of clauses for this function
  uint8_t clause_count;        // Number of clauses
} Function;

// The book of all global functions
typedef struct {
  Function functions[MAX_FUNCTIONS]; // Array of all functions
  uint8_t function_count;            // Number of functions in the book
} Book;

// -----------------------------------------------------------------------------
// HVMN Structure
// -----------------------------------------------------------------------------

// The main HVM-Nano context structure.
// Contains all state needed for term evaluation.
typedef struct {
  // Memory management
  Term* heap;          // Heap memory for terms
  uint32_t heap_size;  // Total size of the heap
  uint32_t heap_pos;   // Current allocation position

  // Evaluation stack
  Term* stack;          // Stack for term evaluation
  uint32_t stack_size;  // Total size of the stack
  uint32_t stack_pos;   // Current stack position

  // Function book
  Book* book;          // Book of global functions

  // Statistics
  uint64_t interactions; // Interaction counter

} HVMN;

// Function declarations to avoid circular dependencies
static inline void hvmn_free(HVMN* hvmn);
static inline Term hvmn_suc_num(HVMN* hvmn, Term suc, Term num);
static inline Term hvmn_suc_sup(HVMN* hvmn, Term suc, Term sup);
static inline Term hvmn_suc_lam(HVMN* hvmn, Term suc, Term lam);

// -----------------------------------------------------------------------------
// Memory Management Functions
// -----------------------------------------------------------------------------

// Create a new HVMN context with the specified heap and stack sizes.
// @param heap_size Number of terms in the heap
// @param stack_size Number of terms in the stack
// @return A new HVMN context or NULL if allocation failed
static inline HVMN* hvmn_new(uint32_t heap_size, uint32_t stack_size) {
  HVMN* hvmn = (HVMN*)malloc(sizeof(HVMN));
  if (!hvmn) return NULL;

  // Initialize structure
  hvmn->heap_size = heap_size;
  hvmn->stack_size = stack_size;
  hvmn->heap_pos = 0;
  hvmn->interactions = 0;
  hvmn->stack_pos = 0;

  // Initialize book
  hvmn->book = (Book*)calloc(1, sizeof(Book));
  if (!hvmn->book) {
    free(hvmn);
    return NULL;
  }

  // Allocate heap and stack
  hvmn->heap = (Term*)calloc(heap_size, sizeof(Term));
  hvmn->stack = (Term*)malloc(stack_size * sizeof(Term));

  if (!hvmn->heap || !hvmn->stack) {
    hvmn_free(hvmn);
    return NULL;
  }

  return hvmn;
}

// Free all resources associated with an HVMN context.
// @param hvmn The HVMN context to free
static inline void hvmn_free(HVMN* hvmn) {
  if (!hvmn) return;

  if (hvmn->heap) free(hvmn->heap);
  if (hvmn->stack) free(hvmn->stack);

  // Free the book and its contents
  if (hvmn->book) {
    for (uint8_t i = 0; i < hvmn->book->function_count; i++) {
      Function* func = &hvmn->book->functions[i];
      for (uint8_t j = 0; j < func->clause_count; j++) {
        if (func->clauses[j].terms) {
          free(func->clauses[j].terms);
        }
      }
    }
    free(hvmn->book);
  }

  free(hvmn);
}

// Allocate n consecutive terms in memory.
// @param hvmn The HVMN context
// @param n Number of terms to allocate
// @return Location in the heap
static inline uint32_t hvmn_alloc(HVMN* hvmn, uint32_t n) {
  if (hvmn->heap_pos + n >= hvmn->heap_size) {
    fprintf(stderr, "Error: Out of memory (tried to allocate %u terms, %u/%u used)\n", n, hvmn->heap_pos, hvmn->heap_size);
    exit(1);
  }

  uint32_t ptr = hvmn->heap_pos;
  hvmn->heap_pos += n;
  return ptr;
}

// -----------------------------------------------------------------------------
// Term Manipulation Functions
// -----------------------------------------------------------------------------

// Create a term with the given tag, label, and value.
// @param tag Term type tag
// @param lab Label value (for SUP, CO0, CO1)
// @param val Value/pointer into the heap
// @return The constructed term
static inline Term hvmn_make_term(TermTag tag, uint8_t lab, uint32_t val) {
  return MAKE_TERM(false, tag, lab, val);
}

// Create a substitution term.
// @param term The term to convert to a substitution
// @return The term with its substitution bit set
static inline Term hvmn_make_sub(Term term) {
  return term | TERM_SUB_MASK;
}

// Remove the substitution bit from a term.
// @param term The term to clear the substitution bit from
// @return The term with its substitution bit cleared
static inline Term hvmn_clear_sub(Term term) {
  return term & ~TERM_SUB_MASK;
}

// -----------------------------------------------------------------------------
// Interaction Functions
// -----------------------------------------------------------------------------

//(λx.f a)
//-------- APP-LAM
//x <- a
//f
static inline Term hvmn_app_lam(HVMN* hvmn, Term app, Term lam) {
  hvmn->interactions++;

  uint32_t app_loc = TERM_VAL(app);
  uint32_t lam_loc = TERM_VAL(lam);

  Term arg = hvmn->heap[app_loc + 1];
  Term bod = hvmn->heap[lam_loc + 0];

  // Create substitution for the lambda variable
  hvmn->heap[lam_loc] = hvmn_make_sub(arg);

  return bod;
}

//(&L{a,b} c)
//----------------- APP-SUP
//! &L{c0,c1} = c;
//&L{(a c0),(b c1)}
static inline Term hvmn_app_sup(HVMN* hvmn, Term app, Term sup) {
  hvmn->interactions++;

  uint32_t app_loc = TERM_VAL(app);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);

  Term arg = hvmn->heap[app_loc + 1];
  Term lft = hvmn->heap[sup_loc + 0];
  Term rgt = hvmn->heap[sup_loc + 1];

  // Allocate only what's necessary
  uint32_t col_loc = hvmn_alloc(hvmn, 1);
  uint32_t app1_loc = hvmn_alloc(hvmn, 2);

  // Store the arg in the collapser location
  hvmn->heap[col_loc] = arg;

  // Create CO0 and CO1 terms
  Term x0 = hvmn_make_term(CO0, sup_lab, col_loc);
  Term x1 = hvmn_make_term(CO1, sup_lab, col_loc);

  // Reuse sup_loc for app0
  hvmn->heap[sup_loc + 1] = x0; // lft is already in heap[sup_loc + 0]

  // Set up app1
  hvmn->heap[app1_loc + 0] = rgt;
  hvmn->heap[app1_loc + 1] = x1;

  // Reuse app_loc for the result superposition
  hvmn->heap[app_loc + 0] = hvmn_make_term(APP, 0, sup_loc);
  hvmn->heap[app_loc + 1] = hvmn_make_term(APP, 0, app1_loc);

  return hvmn_make_term(SUP, sup_lab, app_loc);
}

//! &L{r,s} = λx.f;
//K
//----------------- COL-LAM
//r <- λx0.f0
//s <- λx1.f1
//x <- &L{x0,x1}
//! &L{f0,f1} = f;
//K
static inline Term hvmn_col_lam(HVMN* hvmn, Term col, Term lam) {
  hvmn->interactions++;

  uint32_t col_loc = TERM_VAL(col);
  uint32_t lam_loc = TERM_VAL(lam);
  uint8_t col_lab = TERM_LAB(col);
  uint8_t is_co0 = TERM_TAG(col) == CO0;

  Term bod = hvmn->heap[lam_loc + 0];

  // Batch allocate memory for efficiency
  uint32_t alloc_start = hvmn_alloc(hvmn, 5);
  uint32_t lam0_loc = alloc_start;
  uint32_t lam1_loc = alloc_start + 1;
  uint32_t sup_loc = alloc_start + 2; // 2 locations
  uint32_t col_new_loc = alloc_start + 4;

  // Set up the superposition
  hvmn->heap[sup_loc + 0] = hvmn_make_term(VAR, 0, lam0_loc);
  hvmn->heap[sup_loc + 1] = hvmn_make_term(VAR, 0, lam1_loc);

  // Replace lambda's variable with the superposition
  hvmn->heap[lam_loc] = hvmn_make_sub(hvmn_make_term(SUP, col_lab, sup_loc));

  // Set up the new collapser
  hvmn->heap[col_new_loc] = bod;

  // Set up new lambda bodies
  hvmn->heap[lam0_loc] = hvmn_make_term(CO0, col_lab, col_new_loc);
  hvmn->heap[lam1_loc] = hvmn_make_term(CO1, col_lab, col_new_loc);

  // Create and return the appropriate lambda
  if (is_co0) {
    hvmn->heap[col_loc] = hvmn_make_sub(hvmn_make_term(LAM, 0, lam1_loc));
    return hvmn_make_term(LAM, 0, lam0_loc);
  } else {
    hvmn->heap[col_loc] = hvmn_make_sub(hvmn_make_term(LAM, 0, lam0_loc));
    return hvmn_make_term(LAM, 0, lam1_loc);
  }
}

//! &L{x,y} = &L{a,b};
//K
//-------------------- COL-SUP (if equal labels)
//x <- a
//y <- b
//K

//! &L{x,y} = &R{a,b};
//K
//-------------------- COL-SUP (if different labels)
//x <- &R{a0,b0} 
//y <- &R{a1,b1}
//! &L{a0,a1} = a
//! &L{b0,b1} = b
//K
static inline Term hvmn_col_sup(HVMN* hvmn, Term col, Term sup) {
  hvmn->interactions++;

  uint32_t col_loc = TERM_VAL(col);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t col_lab = TERM_LAB(col);
  uint8_t sup_lab = TERM_LAB(sup);
  uint8_t is_co0 = TERM_TAG(col) == CO0;

  Term lft = hvmn->heap[sup_loc + 0];
  Term rgt = hvmn->heap[sup_loc + 1];

  // Fast path for matching labels (common case)
  if (col_lab == sup_lab) {
    // Labels match: simple substitution
    if (is_co0) {
      hvmn->heap[col_loc] = hvmn_make_sub(rgt);
      return lft;
    } else {
      hvmn->heap[col_loc] = hvmn_make_sub(lft);
      return rgt;
    }
  } else {
    // Labels don't match: create nested collapsers
    uint32_t sup_start = hvmn_alloc(hvmn, 4); // 2 sups with 2 terms each
    uint32_t sup0_loc = sup_start;
    uint32_t sup1_loc = sup_start + 2;

    // Use existing locations as collapser locations
    uint32_t col_lft_loc = sup_loc + 0;
    uint32_t col_rgt_loc = sup_loc + 1;

    // Set up the first superposition (for CO0)
    hvmn->heap[sup0_loc + 0] = hvmn_make_term(CO0, col_lab, col_lft_loc);
    hvmn->heap[sup0_loc + 1] = hvmn_make_term(CO0, col_lab, col_rgt_loc);

    // Set up the second superposition (for CO1)
    hvmn->heap[sup1_loc + 0] = hvmn_make_term(CO1, col_lab, col_lft_loc);
    hvmn->heap[sup1_loc + 1] = hvmn_make_term(CO1, col_lab, col_rgt_loc);

    // Set up original collapsers to point to lft and rgt
    hvmn->heap[col_lft_loc] = lft;
    hvmn->heap[col_rgt_loc] = rgt;

    if (is_co0) {
      hvmn->heap[col_loc] = hvmn_make_sub(hvmn_make_term(SUP, sup_lab, sup1_loc));
      return hvmn_make_term(SUP, sup_lab, sup0_loc);
    } else {
      hvmn->heap[col_loc] = hvmn_make_sub(hvmn_make_term(SUP, sup_lab, sup0_loc));
      return hvmn_make_term(SUP, sup_lab, sup1_loc);
    }
  }
}

// (N a)
// ----- APP-NUM
// ⊥
static inline Term hvmn_app_num(HVMN* hvmn, Term app, Term num) {
  hvmn->interactions++;
  fprintf(stderr, "Runtime error: application of a number\n");
  exit(1);
  return 0; // Unreachable
}

// ! &L{x,y} = N; K
// ---------------- COL-NUM
// x <- N
// y <- N
// K
static inline Term hvmn_col_num(HVMN* hvmn, Term col, Term num) {
  hvmn->interactions++;
  uint32_t col_loc = TERM_VAL(col);
  hvmn->heap[col_loc] = hvmn_make_sub(num);
  return num;
}

// @F(&L{a,b})
// --------------- CAL-SUP
// &L{@F(a) @F(b)}
static inline Term hvmn_cal_sup(HVMN* hvmn, Term cal, Term sup) {
  uint8_t func_id = TERM_LAB(cal);
  
  // Special case for the increment function (SUC)
  if (func_id == SUC) {
    return hvmn_suc_sup(hvmn, cal, sup);
  }
  hvmn->interactions++;
  
  uint32_t cal_loc = TERM_VAL(cal);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  Term a = hvmn->heap[sup_loc + 0];
  Term b = hvmn->heap[sup_loc + 1];
  uint32_t cal_a_loc = hvmn_alloc(hvmn, 1);
  uint32_t cal_b_loc = hvmn_alloc(hvmn, 1);
  hvmn->heap[cal_a_loc] = a;
  hvmn->heap[cal_b_loc] = b;
  Term cal_a = hvmn_make_term(CAL, func_id, cal_a_loc);
  Term cal_b = hvmn_make_term(CAL, func_id, cal_b_loc);
  uint32_t new_sup_loc = hvmn_alloc(hvmn, 2);
  hvmn->heap[new_sup_loc + 0] = cal_a;
  hvmn->heap[new_sup_loc + 1] = cal_b;
  return hvmn_make_term(SUP, sup_lab, new_sup_loc);
}

// @F(λx.f)
// -------- CAL-LAM
// ⊥
static inline Term hvmn_cal_lam(HVMN* hvmn, Term cal, Term lam) {
  uint8_t func_id = TERM_LAB(cal);
  
  // Special case for the increment function (SUC)
  if (func_id == SUC) {
    return hvmn_suc_lam(hvmn, cal, lam);
  }
  hvmn->interactions++;
  
  fprintf(stderr, "Runtime error: function call on a lambda\n");
  exit(1);
  return 0; // Unreachable
}

// @F(N)
// ---------------- CAL-NUM
// deref(F)[x <- N]
static inline Term hvmn_cal_num(HVMN* hvmn, Term cal, Term num) {
  uint8_t func_id = TERM_LAB(cal);
  
  // Special case for the increment function (SUC)
  if (func_id == SUC) {
    return hvmn_suc_num(hvmn, cal, num);
  }
  hvmn->interactions++;
  
  uint32_t N = TERM_VAL(num);
  
  if (func_id >= hvmn->book->function_count) {
    fprintf(stderr, "Runtime error: invalid function id %u\n", func_id);
    exit(1);
  }
  
  Function* func = &hvmn->book->functions[func_id];
  uint8_t M = func->clause_count;
  
  if (M == 0) {
    fprintf(stderr, "Runtime error: function %u has no clauses\n", func_id);
    exit(1);
  }
  
  uint8_t clause_index;
  bool replace_var = false;
  uint32_t var_value = 0;
  
  if (N < M - 1) {
    clause_index = (uint8_t)N;
  } else {
    clause_index = M - 1;
    replace_var = true;
    var_value = N - (M - 1);
  }
  
  Clause* clause = &func->clauses[clause_index];
  uint32_t term_count = clause->term_count;
  uint32_t L = hvmn_alloc(hvmn, term_count);
  
  for (uint32_t i = 0; i < term_count; i++) {
    Term orig_term = clause->terms[i];
    TermTag tag = TERM_TAG(orig_term);
    uint8_t lab = TERM_LAB(orig_term);
    uint32_t val = TERM_VAL(orig_term);
    
    if (replace_var && tag == VAR && val == PATTERN_VAR_MASK) {
      hvmn->heap[L + i] = hvmn_make_term(NAT, 0, var_value);
    } else if (tag != NAT) {
      hvmn->heap[L + i] = MAKE_TERM(false, tag, lab, L + val);
    } else {
      hvmn->heap[L + i] = orig_term;
    }
  }
  
  return hvmn->heap[L];
}

// +N
// --- SUC-NUM
// N+1
static inline Term hvmn_suc_num(HVMN* hvmn, Term suc, Term num) {
  hvmn->interactions++;
  uint32_t N = TERM_VAL(num);
  return hvmn_make_term(NAT, 0, N + 1);
}

// +{x,y}
// ------- SUC-SUP
// {+x,+y}
static inline Term hvmn_suc_sup(HVMN* hvmn, Term suc, Term sup) {
  hvmn->interactions++;
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  Term x = hvmn->heap[sup_loc + 0];
  Term y = hvmn->heap[sup_loc + 1];
  uint32_t suc_x_loc = hvmn_alloc(hvmn, 1);
  uint32_t suc_y_loc = hvmn_alloc(hvmn, 1);
  hvmn->heap[suc_x_loc] = x;
  hvmn->heap[suc_y_loc] = y;
  Term suc_x = hvmn_make_term(CAL, SUC, suc_x_loc);
  Term suc_y = hvmn_make_term(CAL, SUC, suc_y_loc);
  uint32_t new_sup_loc = hvmn_alloc(hvmn, 2);
  hvmn->heap[new_sup_loc + 0] = suc_x;
  hvmn->heap[new_sup_loc + 1] = suc_y;
  return hvmn_make_term(SUP, sup_lab, new_sup_loc);
}

// +λx.f
// ----- SUC-LAM
// ⊥
static inline Term hvmn_suc_lam(HVMN* hvmn, Term suc, Term lam) {
  hvmn->interactions++;
  fprintf(stderr, "Runtime error: successor of a lambda\n");
  exit(1);
  return 0; // Unreachable
}

// -----------------------------------------------------------------------------
// Term Normalization
// -----------------------------------------------------------------------------

// Reduce a term to weak head normal form (WHNF).
// WHNF means reducing until the outermost constructor is a value (LAM, SUP, etc.),
// or until no more reductions are possible.
// 
// @param hvmn The HVMN context
// @param term The term to reduce
// @return The term in WHNF
static inline Term hvmn_whnf(HVMN* hvmn, Term term) {
  uint32_t stop = hvmn->stack_pos;
  Term next = term;
  Term* heap = hvmn->heap;
  Term* stack = hvmn->stack;
  uint32_t stack_pos = stop;

  while (1) {
    TermTag tag = TERM_TAG(next);

    switch (tag) {
      case VAR: {
        uint32_t var_loc = TERM_VAL(next);
        Term subst = heap[var_loc];
        if (TERM_SUB(subst)) {
          next = hvmn_clear_sub(subst);
          continue;
        }
        break; // No substitution, so it's in WHNF
      }

      case CO0:
      case CO1: {
        uint32_t col_loc = TERM_VAL(next);
        Term val = heap[col_loc];
        if (TERM_SUB(val)) {
          next = hvmn_clear_sub(val);
          continue;
        } else {
          stack[stack_pos++] = next;
          next = val;
          continue;
        }
      }

      case APP: case CAL: {
        uint32_t app_loc = TERM_VAL(next);
        stack[stack_pos++] = next;
        next = heap[app_loc]; // Reduce the function part
        continue;
      }

      default: {
        if (stack_pos == stop) {
          hvmn->stack_pos = stack_pos; // Update stack position before return
          return next; // Stack empty, term is in WHNF
        // Interaction Dispatcher
        } else {
          Term prev = stack[--stack_pos];
          TermTag ptag = TERM_TAG(prev);
          
          // Use switch for more efficient dispatching
          switch (ptag) {
            case APP:
              switch (tag) {
                case LAM: next = hvmn_app_lam(hvmn, prev, next); continue;
                case SUP: next = hvmn_app_sup(hvmn, prev, next); continue;
                case NAT: next = hvmn_app_num(hvmn, prev, next); continue;
                default: break;
              }
              break;
              
            case CO0:
            case CO1:
              switch (tag) {
                case LAM: next = hvmn_col_lam(hvmn, prev, next); continue;
                case SUP: next = hvmn_col_sup(hvmn, prev, next); continue;
                case NAT: next = hvmn_col_num(hvmn, prev, next); continue;
                default: break;
              }
              break;
              
            case CAL:
              switch (tag) {
                case LAM: next = hvmn_cal_lam(hvmn, prev, next); continue;
                case SUP: next = hvmn_cal_sup(hvmn, prev, next); continue;
                case NAT: next = hvmn_cal_num(hvmn, prev, next); continue;
                default: break;
              }
              break;
              
            default:
              break;
          }

          // No interaction found for benchmark code, proceed to stack traversal
          stack[stack_pos++] = prev;
          break;
        }
      }
    }

    // After processing, check stack and update heap if needed
    if (stack_pos == stop) {
      hvmn->stack_pos = stack_pos;
      return next; // Stack empty, return WHNF
    } else {
      while (stack_pos > stop) {
        Term host = stack[--stack_pos];
        TermTag htag = TERM_TAG(host);
        uint32_t hloc = TERM_VAL(host);

        // Update the heap with the reduced term
        if (htag == APP || htag == CO0 || htag == CO1 || htag == CAL) {
          heap[hloc] = next;
        }
        next = host;
      }
      hvmn->stack_pos = stack_pos;
      return next; // Return updated original term
    }
  }
}


// Reduce a term to full normal form by recursively applying WHNF
// to all subterms.
// 
// @param hvmn The HVMN context
// @param term The term to normalize
// @return The fully normalized term
static inline Term hvmn_normal(HVMN* hvmn, Term term) {
  // Reset stack
  hvmn->stack_pos = 0;
  Term* heap = hvmn->heap;
  Term* stack = hvmn->stack;
  uint32_t stack_pos = 0;

  // Allocate a new node for the initial term
  uint32_t root_loc = hvmn_alloc(hvmn, 1);
  heap[root_loc] = term;

  // Push initial location to stack as a "location"
  stack[stack_pos++] = MAKE_TERM(false, 0, 0, root_loc);

  while (stack_pos > 0) {
    // Pop current location from stack
    uint32_t loc = TERM_VAL(stack[--stack_pos]);

    // Get term at this location
    Term current = heap[loc];

    // Reduce to WHNF
    hvmn->stack_pos = stack_pos;
    current = hvmn_whnf(hvmn, current);
    stack_pos = hvmn->stack_pos;

    // Store the WHNF term back to the heap
    heap[loc] = current;

    // Get term details
    TermTag tag = TERM_TAG(current);
    uint32_t val = TERM_VAL(current);

    // Push subterm locations based on term type
    if (tag == LAM) {
      stack[stack_pos++] = MAKE_TERM(false, 0, 0, val);
    }
    else if (tag == APP || tag == SUP) {
      // Both APP and SUP need to push two locations
      stack[stack_pos++] = MAKE_TERM(false, 0, 0, val);
      stack[stack_pos++] = MAKE_TERM(false, 0, 0, val + 1);
    }
  }

  // Update stack position and return the fully normalized term
  hvmn->stack_pos = stack_pos;
  return heap[root_loc];
}

// Create a new HVMN context with default heap and stack sizes.
// @return A new HVMN context or NULL if allocation failed
static inline HVMN* hvmn_default_new() {
  return hvmn_new(HVMN_DEFAULT_HEAP_SIZE, HVMN_DEFAULT_STACK_SIZE);
}

#endif // HVMN_H
