//./../HVM-Nano.md//
//./parse.h//
//./parse.c//

#ifndef IC_H
#define IC_H

/**
 * Interaction Calculus (IC) - Core header-only implementation
 * 
 * This file contains the full implementation of the Interaction Calculus:
 * - Term representation and bit manipulation
 * - Memory management
 * - Core interactions (app_lam, app_sup, col_lam, col_sup, etc.)
 * - Weak Head Normal Form (WHNF) reduction
 * - Full Normal Form reduction
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Default heap and stack sizes
#define IC_DEFAULT_HEAP_SIZE (1 << 28) // 256M terms
#define IC_DEFAULT_STACK_SIZE (1 << 24) // 16M terms

// -----------------------------------------------------------------------------
// Core Types and Constants
// -----------------------------------------------------------------------------

// Term tags
typedef enum {
  VAR, // Variable
  SUP, // Superposition
  CO0, // Collapser first variable
  CO1, // Collapser second variable
  LAM, // Lambda
  APP, // Application
  NAT, // Natural number (NUM or SUC)
  CAL  // Function call
} TermTag;

// Term 32-bit packed representation
typedef uint32_t Term;

// Term components
#define TERM_SUB_MASK 0x80000000UL // 1-bit: Is this a substitution?
#define TERM_TAG_MASK 0x70000000UL // 3-bits: Term tag
#define TERM_LAB_MASK 0x0F000000UL // 4-bits: Label for superpositions/functions
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

/**
 * Represents a single clause in a function
 */
typedef struct {
  Term* terms;         // Array of terms in the clause body
  uint32_t term_count; // Number of terms in the clause
} Clause;

/**
 * Represents a global function with pattern-matching clauses
 */
typedef struct {
  Clause clauses[MAX_CLAUSES]; // Array of clauses for this function
  uint8_t clause_count;        // Number of clauses
} Function;

/**
 * The book of all global functions
 */
typedef struct {
  Function functions[MAX_FUNCTIONS]; // Array of all functions
  uint8_t function_count;            // Number of functions in the book
} Book;

// -----------------------------------------------------------------------------
// IC Structure
// -----------------------------------------------------------------------------

/**
 * The main Interaction Calculus context structure.
 * Contains all state needed for term evaluation.
 */
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
} IC;

// Function declarations to avoid circular dependencies
static inline void ic_free(IC* ic);

// -----------------------------------------------------------------------------
// Memory Management Functions
// -----------------------------------------------------------------------------

/**
 * Create a new IC context with the specified heap and stack sizes.
 * @param heap_size Number of terms in the heap
 * @param stack_size Number of terms in the stack
 * @return A new IC context or NULL if allocation failed
 */
static inline IC* ic_new(uint32_t heap_size, uint32_t stack_size) {
  IC* ic = (IC*)malloc(sizeof(IC));
  if (!ic) return NULL;

  // Initialize structure
  ic->heap_size = heap_size;
  ic->stack_size = stack_size;
  ic->heap_pos = 0;
  ic->interactions = 0;
  ic->stack_pos = 0;

  // Initialize book
  ic->book = (Book*)calloc(1, sizeof(Book));
  if (!ic->book) {
    free(ic);
    return NULL;
  }

  // Allocate heap and stack
  ic->heap = (Term*)calloc(heap_size, sizeof(Term));
  ic->stack = (Term*)malloc(stack_size * sizeof(Term));

  if (!ic->heap || !ic->stack) {
    ic_free(ic);
    return NULL;
  }

  return ic;
}

/**
 * Free all resources associated with an IC context.
 * @param ic The IC context to free
 */
static inline void ic_free(IC* ic) {
  if (!ic) return;

  if (ic->heap) free(ic->heap);
  if (ic->stack) free(ic->stack);

  // Free the book and its contents
  if (ic->book) {
    for (uint8_t i = 0; i < ic->book->function_count; i++) {
      Function* func = &ic->book->functions[i];
      for (uint8_t j = 0; j < func->clause_count; j++) {
        if (func->clauses[j].terms) {
          free(func->clauses[j].terms);
        }
      }
    }
    free(ic->book);
  }

  free(ic);
}

/**
 * Allocate n consecutive terms in memory.
 * @param ic The IC context
 * @param n Number of terms to allocate
 * @return Location in the heap
 */
static inline uint32_t ic_alloc(IC* ic, uint32_t n) {
  if (ic->heap_pos + n >= ic->heap_size) {
    fprintf(stderr, "Error: Out of memory (tried to allocate %u terms, %u/%u used)\n", n, ic->heap_pos, ic->heap_size);
    exit(1);
  }

  uint32_t ptr = ic->heap_pos;
  ic->heap_pos += n;
  return ptr;
}

// -----------------------------------------------------------------------------
// Term Manipulation Functions
// -----------------------------------------------------------------------------

/**
 * Create a term with the given tag, label, and value.
 * @param tag Term type tag
 * @param lab Label value (for SUP, CO0, CO1, CAL, NAT)
 * @param val Value/pointer into the heap
 * @return The constructed term
 */
static inline Term ic_make_term(TermTag tag, uint8_t lab, uint32_t val) {
  return MAKE_TERM(false, tag, lab, val);
}

/**
 * Create a substitution term.
 * @param term The term to convert to a substitution
 * @return The term with its substitution bit set
 */
static inline Term ic_make_sub(Term term) {
  return term | TERM_SUB_MASK;
}

/**
 * Remove the substitution bit from a term.
 * @param term The term to clear the substitution bit from
 * @return The term with its substitution bit cleared
 */
static inline Term ic_clear_sub(Term term) {
  return term & ~TERM_SUB_MASK;
}

// -----------------------------------------------------------------------------
// Interaction Functions
// -----------------------------------------------------------------------------

// Error handling for interactions resulting in ⊥
static inline Term ic_error(const char* msg) {
  fprintf(stderr, "Runtime error: %s\n", msg);
  exit(1);
  return 0; // Unreachable
}

// (λx.f a)
// -------- APP-LAM
// x <- a
// f
static inline Term ic_app_lam(IC* ic, Term app, Term lam) {
  ic->interactions++;
  uint32_t app_loc = TERM_VAL(app);
  uint32_t lam_loc = TERM_VAL(lam);
  Term arg = ic->heap[app_loc + 1];
  Term bod = ic->heap[lam_loc + 0];
  ic->heap[lam_loc] = ic_make_sub(arg);
  return bod;
}

// (&L{a,b} c)
// ----------------- APP-SUP
// ! &L{c0,c1} = c;
// &L{(a c0),(b c1)}
static inline Term ic_app_sup(IC* ic, Term app, Term sup) {
  ic->interactions++;
  uint32_t app_loc = TERM_VAL(app);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  Term arg = ic->heap[app_loc + 1];
  Term lft = ic->heap[sup_loc + 0];
  Term rgt = ic->heap[sup_loc + 1];
  uint32_t col_loc = ic_alloc(ic, 1);
  uint32_t app1_loc = ic_alloc(ic, 2);
  ic->heap[col_loc] = arg;
  Term x0 = ic_make_term(CO0, sup_lab, col_loc);
  Term x1 = ic_make_term(CO1, sup_lab, col_loc);
  ic->heap[sup_loc + 1] = x0; // Reuse sup_loc for app0
  ic->heap[app1_loc + 0] = rgt;
  ic->heap[app1_loc + 1] = x1;
  ic->heap[app_loc + 0] = ic_make_term(APP, 0, sup_loc);
  ic->heap[app_loc + 1] = ic_make_term(APP, 0, app1_loc);
  return ic_make_term(SUP, sup_lab, app_loc);
}

// (N a)
// ----- APP-NUM
// ⊥
static inline Term ic_app_num(IC* ic, Term app, Term num) {
  ic->interactions++;
  return ic_error("application of a number");
}

// ! &L{r,s} = λx.f; K
// ----------------- COL-LAM
// r <- λx0.f0
// s <- λx1.f1
// x <- &L{x0,x1}
// ! &L{f0,f1} = f; K
static inline Term ic_col_lam(IC* ic, Term col, Term lam) {
  ic->interactions++;
  uint32_t col_loc = TERM_VAL(col);
  uint32_t lam_loc = TERM_VAL(lam);
  uint8_t col_lab = TERM_LAB(col);
  uint8_t is_co0 = TERM_TAG(col) == CO0;
  Term bod = ic->heap[lam_loc + 0];
  uint32_t alloc_start = ic_alloc(ic, 5);
  uint32_t lam0_loc = alloc_start;
  uint32_t lam1_loc = alloc_start + 1;
  uint32_t sup_loc = alloc_start + 2;
  uint32_t col_new_loc = alloc_start + 4;
  ic->heap[sup_loc + 0] = ic_make_term(VAR, 0, lam0_loc);
  ic->heap[sup_loc + 1] = ic_make_term(VAR, 0, lam1_loc);
  ic->heap[lam_loc] = ic_make_sub(ic_make_term(SUP, col_lab, sup_loc));
  ic->heap[col_new_loc] = bod;
  ic->heap[lam0_loc] = ic_make_term(CO0, col_lab, col_new_loc);
  ic->heap[lam1_loc] = ic_make_term(CO1, col_lab, col_new_loc);
  if (is_co0) {
    ic->heap[col_loc] = ic_make_sub(ic_make_term(LAM, 0, lam1_loc));
    return ic_make_term(LAM, 0, lam0_loc);
  } else {
    ic->heap[col_loc] = ic_make_sub(ic_make_term(LAM, 0, lam0_loc));
    return ic_make_term(LAM, 0, lam1_loc);
  }
}

// ! &L{x,y} = &L{a,b}; K
// -------------------- COL-SUP (if equal labels)
// x <- a
// y <- b
// K
// 
// ! &L{x,y} = &R{a,b}; K
// -------------------- COL-SUP (if different labels)
// x <- &R{a0,b0} 
// y <- &R{a1,b1}
// ! &L{a0,a1} = a;
// ! &L{b0,b1} = b;
// K
static inline Term ic_col_sup(IC* ic, Term col, Term sup) {
  ic->interactions++;
  uint32_t col_loc = TERM_VAL(col);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t col_lab = TERM_LAB(col);
  uint8_t sup_lab = TERM_LAB(sup);
  uint8_t is_co0 = TERM_TAG(col) == CO0;
  Term lft = ic->heap[sup_loc + 0];
  Term rgt = ic->heap[sup_loc + 1];
  if (col_lab == sup_lab) {
    if (is_co0) {
      ic->heap[col_loc] = ic_make_sub(rgt);
      return lft;
    } else {
      ic->heap[col_loc] = ic_make_sub(lft);
      return rgt;
    }
  } else {
    uint32_t sup_start = ic_alloc(ic, 4);
    uint32_t sup0_loc = sup_start;
    uint32_t sup1_loc = sup_start + 2;
    uint32_t col_lft_loc = sup_loc + 0;
    uint32_t col_rgt_loc = sup_loc + 1;
    ic->heap[sup0_loc + 0] = ic_make_term(CO0, col_lab, col_lft_loc);
    ic->heap[sup0_loc + 1] = ic_make_term(CO0, col_lab, col_rgt_loc);
    ic->heap[sup1_loc + 0] = ic_make_term(CO1, col_lab, col_lft_loc);
    ic->heap[sup1_loc + 1] = ic_make_term(CO1, col_lab, col_rgt_loc);
    ic->heap[col_lft_loc] = lft;
    ic->heap[col_rgt_loc] = rgt;
    if (is_co0) {
      ic->heap[col_loc] = ic_make_sub(ic_make_term(SUP, sup_lab, sup1_loc));
      return ic_make_term(SUP, sup_lab, sup0_loc);
    } else {
      ic->heap[col_loc] = ic_make_sub(ic_make_term(SUP, sup_lab, sup0_loc));
      return ic_make_term(SUP, sup_lab, sup1_loc);
    }
  }
}

// ! &L{x,y} = N; K
// ---------------- COL-NUM
// x <- N
// y <- N
// K
static inline Term ic_col_num(IC* ic, Term col, Term num) {
  ic->interactions++;
  uint32_t col_loc = TERM_VAL(col);
  ic->heap[col_loc] = ic_make_sub(num);
  return num;
}

// @F(&L{a,b})
// --------------- CAL-SUP
// &L{@F(a) @F(b)}
static inline Term ic_cal_sup(IC* ic, Term cal, Term sup) {
  ic->interactions++;
  uint32_t cal_loc = TERM_VAL(cal);
  uint8_t func_id = TERM_LAB(cal);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  Term a = ic->heap[sup_loc + 0];
  Term b = ic->heap[sup_loc + 1];
  uint32_t cal_a_loc = ic_alloc(ic, 1);
  uint32_t cal_b_loc = ic_alloc(ic, 1);
  ic->heap[cal_a_loc] = a;
  ic->heap[cal_b_loc] = b;
  Term cal_a = ic_make_term(CAL, func_id, cal_a_loc);
  Term cal_b = ic_make_term(CAL, func_id, cal_b_loc);
  uint32_t new_sup_loc = ic_alloc(ic, 2);
  ic->heap[new_sup_loc + 0] = cal_a;
  ic->heap[new_sup_loc + 1] = cal_b;
  return ic_make_term(SUP, sup_lab, new_sup_loc);
}

// @F(N)
// ---------------- CAL-NUM
// deref(F)[x <- N]
static inline Term ic_cal_num(IC* ic, Term cal, Term num) {
  ic->interactions++;
  uint8_t func_id = TERM_LAB(cal);
  uint32_t N = TERM_VAL(num);
  Function* func = &ic->book->functions[func_id];
  if (func_id >= ic->book->function_count || func->clause_count == 0) {
    fprintf(stderr, "Error: function %c not defined\n", 'A' + func_id);
    exit(1);
  }
  uint8_t clause_index = N < func->clause_count - 1 ? N : func->clause_count - 1;
  Clause* clause = &func->clauses[clause_index];
  uint32_t term_count = clause->term_count;
  if (term_count == 0) {
    fprintf(stderr, "Error: empty clause\n");
    exit(1);
  }
  
  // Allocate space for the entire clause
  uint32_t loc = ic_alloc(ic, term_count);
  
  // Calculate pattern variable value for recursive function pattern (N-(K-1))
  uint32_t x = (clause_index == func->clause_count - 1) ? N - clause_index : 0;
  
  // Copy all terms from the clause to the allocated space
  for (uint32_t i = 0; i < term_count; i++) {
    Term term = clause->terms[i];
    TermTag tag = TERM_TAG(term);
    uint8_t lab = TERM_LAB(term);
    uint32_t val = TERM_VAL(term);
    
    // Replace the pattern variable with its computed value
    if (tag == VAR && val == PATTERN_VAR_MASK) {
      if (clause_index == func->clause_count - 1) {
        ic->heap[loc + i] = ic_make_term(NAT, 0, x);
      } else {
        fprintf(stderr, "Error: pattern variable in non-last clause\n");
        exit(1);
      }
    } 
    // Adjust pointer values to point to the new memory location
    else if (tag == VAR || tag == LAM || tag == APP || tag == SUP || tag == CAL || (tag == NAT && lab == 1)) {
      // The pointers in the stored clause are relative to the start of the clause,
      // so we add 'loc' to make them point to the correct absolute position
      ic->heap[loc + i] = ic_make_term(tag, lab, val + loc);
    } 
    // For non-pointer terms, just copy them as-is
    else {
      ic->heap[loc + i] = term;
    }
  }
  
  return ic->heap[loc];
}

// @F(λx.f)
// -------- CAL-LAM
// ⊥
static inline Term ic_cal_lam(IC* ic, Term cal, Term lam) {
  ic->interactions++;
  return ic_error("function call on a lambda");
}

// +N
// --- SUC-NUM
// N+1
static inline Term ic_suc_num(IC* ic, Term suc, Term num) {
  ic->interactions++;
  uint32_t N = TERM_VAL(num);
  return ic_make_term(NAT, 0, N + 1);
}

// +{x,y}
// ------- SUC-SUP
// {+x,+y}
static inline Term ic_suc_sup(IC* ic, Term suc, Term sup) {
  ic->interactions++;
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  Term x = ic->heap[sup_loc + 0];
  Term y = ic->heap[sup_loc + 1];
  uint32_t suc_x_loc = ic_alloc(ic, 1);
  uint32_t suc_y_loc = ic_alloc(ic, 1);
  ic->heap[suc_x_loc] = x;
  ic->heap[suc_y_loc] = y;
  Term suc_x = ic_make_term(NAT, 1, suc_x_loc);
  Term suc_y = ic_make_term(NAT, 1, suc_y_loc);
  uint32_t new_sup_loc = ic_alloc(ic, 2);
  ic->heap[new_sup_loc + 0] = suc_x;
  ic->heap[new_sup_loc + 1] = suc_y;
  return ic_make_term(SUP, sup_lab, new_sup_loc);
}

// +λx.f
// ----- SUC-LAM
// ⊥
static inline Term ic_suc_lam(IC* ic, Term suc, Term lam) {
  ic->interactions++;
  return ic_error("successor of a lambda");
}

// -----------------------------------------------------------------------------
// Term Normalization
// -----------------------------------------------------------------------------

/**
 * Reduce a term to weak head normal form (WHNF).
 * WHNF means reducing until the outermost constructor is a value (LAM, SUP, etc.),
 * or until no more reductions are possible.
 * 
 * @param ic The IC context
 * @param term The term to reduce
 * @return The term in WHNF
 */

static inline Term ic_whnf(IC* ic, Term term) {
  uint32_t stop = ic->stack_pos;
  Term next = term;
  Term* heap = ic->heap;
  Term* stack = ic->stack;
  uint32_t stack_pos = stop;

  while (1) {
    TermTag tag = TERM_TAG(next);
    bool is_ctr = false;

    switch (tag) {
      case VAR: {
        uint32_t var_loc = TERM_VAL(next);
        Term subst = heap[var_loc];
        if (TERM_SUB(subst)) {
          next = ic_clear_sub(subst);
          continue;
        } else {
          is_ctr = true;
        }
        break;
      }

      case CO0:
      case CO1: {
        uint32_t col_loc = TERM_VAL(next);
        Term val = heap[col_loc];
        if (TERM_SUB(val)) {
          next = ic_clear_sub(val);
          continue;
        } else {
          stack[stack_pos++] = next;
          next = val;
          continue;
        }
      }

      case APP: {
        uint32_t app_loc = TERM_VAL(next);
        stack[stack_pos++] = next;
        next = heap[app_loc]; // Reduce the function
        continue;
      }

      case CAL: {
        uint32_t cal_loc = TERM_VAL(next);
        stack[stack_pos++] = next;
        next = heap[cal_loc]; // Reduce the argument
        continue;
      }

      case NAT: {
        if (TERM_LAB(next) == 0) {
          is_ctr = true;
        } else { // label == 1, SUC
          uint32_t suc_loc = TERM_VAL(next);
          stack[stack_pos++] = next;
          next = heap[suc_loc]; // Reduce the argument
          continue;
        }
        break;
      }

      default: { // LAM, SUP
        is_ctr = true;
        break;
      }
    }

    if (is_ctr) {
      if (stack_pos == stop) {
        ic->stack_pos = stack_pos;
        return next;
      } else {
        Term prev = stack[--stack_pos];
        TermTag ptag = TERM_TAG(prev);
        TermTag ttag = TERM_TAG(next);

        // Handle interactions
        if (ptag == APP && ttag == LAM) {
          next = ic_app_lam(ic, prev, next);
          continue;
        } else if (ptag == APP && ttag == SUP) {
          next = ic_app_sup(ic, prev, next);
          continue;
        } else if (ptag == APP && ttag == NAT && TERM_LAB(next) == 0) {
          next = ic_app_num(ic, prev, next);
          continue;
        } else if ((ptag == CO0 || ptag == CO1) && ttag == LAM) {
          next = ic_col_lam(ic, prev, next);
          continue;
        } else if ((ptag == CO0 || ptag == CO1) && ttag == SUP) {
          next = ic_col_sup(ic, prev, next);
          continue;
        } else if ((ptag == CO0 || ptag == CO1) && ttag == NAT && TERM_LAB(next) == 0) {
          next = ic_col_num(ic, prev, next);
          continue;
        } else if (ptag == CAL && ttag == SUP) {
          next = ic_cal_sup(ic, prev, next);
          continue;
        } else if (ptag == CAL && ttag == NAT && TERM_LAB(next) == 0) {
          next = ic_cal_num(ic, prev, next);
          continue;
        } else if (ptag == CAL && ttag == LAM) {
          next = ic_cal_lam(ic, prev, next);
          continue;
        } else if (ptag == NAT && TERM_LAB(prev) == 1 && ttag == NAT && TERM_LAB(next) == 0) {
          next = ic_suc_num(ic, prev, next);
          continue;
        } else if (ptag == NAT && TERM_LAB(prev) == 1 && ttag == SUP) {
          next = ic_suc_sup(ic, prev, next);
          continue;
        } else if (ptag == NAT && TERM_LAB(prev) == 1 && ttag == LAM) {
          next = ic_suc_lam(ic, prev, next);
          continue;
        } else {
          // No interaction, push prev back
          stack[stack_pos++] = prev;
        }
      }
    }

    // Traverse the stack to update the heap
    while (stack_pos > stop) {
      Term host = stack[--stack_pos];
      TermTag htag = TERM_TAG(host);
      uint32_t hloc = TERM_VAL(host);
      if (htag == APP || htag == CO0 || htag == CO1 || htag == CAL || (htag == NAT && TERM_LAB(host) == 1)) {
        heap[hloc] = next;
      }
      next = host;
    }
    ic->stack_pos = stack_pos;
    return next;
  }
}

/**
 * Reduce a term to full normal form by recursively applying WHNF
 * to all subterms.
 * 
 * @param ic The IC context
 * @param term The term to normalize
 * @return The fully normalized term
 */
static inline Term ic_normal(IC* ic, Term term) {
  ic->stack_pos = 0;
  Term* heap = ic->heap;
  Term* stack = ic->stack;
  uint32_t stack_pos = 0;
  uint32_t root_loc = ic_alloc(ic, 1);
  heap[root_loc] = term;
  stack[stack_pos++] = MAKE_TERM(false, 0, 0, root_loc);

  while (stack_pos > 0) {
    uint32_t loc = TERM_VAL(stack[--stack_pos]);
    Term current = heap[loc];
    ic->stack_pos = stack_pos;
    current = ic_whnf(ic, current);
    stack_pos = ic->stack_pos;
    heap[loc] = current;
    TermTag tag = TERM_TAG(current);
    uint32_t val = TERM_VAL(current);
    if (tag == LAM) {
      stack[stack_pos++] = MAKE_TERM(false, 0, 0, val);
    } else if (tag == APP || tag == SUP) {
      stack[stack_pos++] = MAKE_TERM(false, 0, 0, val);
      stack[stack_pos++] = MAKE_TERM(false, 0, 0, val + 1);
    } else if (tag == NAT && TERM_LAB(current) == 1) {
      stack[stack_pos++] = MAKE_TERM(false, 0, 0, val);
    } else if (tag == CAL) {
      stack[stack_pos++] = MAKE_TERM(false, 0, 0, val);
    }
  }

  ic->stack_pos = stack_pos;
  return heap[root_loc];
}

/**
 * Create a new IC context with default heap and stack sizes.
 * @return A new IC context or NULL if allocation failed
 */
static inline IC* ic_default_new() {
  return ic_new(IC_DEFAULT_HEAP_SIZE, IC_DEFAULT_STACK_SIZE);
}

#endif // IC_H
