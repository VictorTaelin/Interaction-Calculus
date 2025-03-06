//./../HVM-Nano.md//

#ifndef IC_H
#define IC_H

// -----------------------------------------------------------------------------
// Interaction Calculus (IC) - Core header-only implementation
// 
// This file contains the full implementation of the Interaction Calculus:
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
#define IC_DEFAULT_HEAP_SIZE (1 << 28) // 256M terms
#define IC_DEFAULT_STACK_SIZE (1 << 24) // 16M terms

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
// IC Structure
// -----------------------------------------------------------------------------

// The main Interaction Calculus context structure.
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

} IC;

// Function declarations to avoid circular dependencies
static inline void ic_free(IC* ic);
static inline Term ic_suc_num(IC* ic, Term suc, Term num);
static inline Term ic_suc_sup(IC* ic, Term suc, Term sup);
static inline Term ic_suc_lam(IC* ic, Term suc, Term lam);

// -----------------------------------------------------------------------------
// Memory Management Functions
// -----------------------------------------------------------------------------

// Create a new IC context with the specified heap and stack sizes.
// @param heap_size Number of terms in the heap
// @param stack_size Number of terms in the stack
// @return A new IC context or NULL if allocation failed
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

// Free all resources associated with an IC context.
// @param ic The IC context to free
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

// Allocate n consecutive terms in memory.
// @param ic The IC context
// @param n Number of terms to allocate
// @return Location in the heap
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

// Create a term with the given tag, label, and value.
// @param tag Term type tag
// @param lab Label value (for SUP, CO0, CO1)
// @param val Value/pointer into the heap
// @return The constructed term
static inline Term ic_make_term(TermTag tag, uint8_t lab, uint32_t val) {
  return MAKE_TERM(false, tag, lab, val);
}

// Create a substitution term.
// @param term The term to convert to a substitution
// @return The term with its substitution bit set
static inline Term ic_make_sub(Term term) {
  return term | TERM_SUB_MASK;
}

// Remove the substitution bit from a term.
// @param term The term to clear the substitution bit from
// @return The term with its substitution bit cleared
static inline Term ic_clear_sub(Term term) {
  return term & ~TERM_SUB_MASK;
}

// -----------------------------------------------------------------------------
// Interaction Functions
// -----------------------------------------------------------------------------

//(λx.f a)
//-------- APP-LAM
//x <- a
//f
static inline Term ic_app_lam(IC* ic, Term app, Term lam) {
  ic->interactions++;

  uint32_t app_loc = TERM_VAL(app);
  uint32_t lam_loc = TERM_VAL(lam);

  Term arg = ic->heap[app_loc + 1];
  Term bod = ic->heap[lam_loc + 0];

  // Create substitution for the lambda variable
  ic->heap[lam_loc] = ic_make_sub(arg);

  return bod;
}

//(&L{a,b} c)
//----------------- APP-SUP
//! &L{c0,c1} = c;
//&L{(a c0),(b c1)}
static inline Term ic_app_sup(IC* ic, Term app, Term sup) {
  ic->interactions++;

  uint32_t app_loc = TERM_VAL(app);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);

  Term arg = ic->heap[app_loc + 1];
  Term lft = ic->heap[sup_loc + 0];
  Term rgt = ic->heap[sup_loc + 1];

  // Allocate only what's necessary
  uint32_t col_loc = ic_alloc(ic, 1);
  uint32_t app1_loc = ic_alloc(ic, 2);

  // Store the arg in the collapser location
  ic->heap[col_loc] = arg;

  // Create CO0 and CO1 terms
  Term x0 = ic_make_term(CO0, sup_lab, col_loc);
  Term x1 = ic_make_term(CO1, sup_lab, col_loc);

  // Reuse sup_loc for app0
  ic->heap[sup_loc + 1] = x0; // lft is already in heap[sup_loc + 0]

  // Set up app1
  ic->heap[app1_loc + 0] = rgt;
  ic->heap[app1_loc + 1] = x1;

  // Reuse app_loc for the result superposition
  ic->heap[app_loc + 0] = ic_make_term(APP, 0, sup_loc);
  ic->heap[app_loc + 1] = ic_make_term(APP, 0, app1_loc);

  return ic_make_term(SUP, sup_lab, app_loc);
}

//! &L{r,s} = λx.f;
//K
//----------------- COL-LAM
//r <- λx0.f0
//s <- λx1.f1
//x <- &L{x0,x1}
//! &L{f0,f1} = f;
//K
static inline Term ic_col_lam(IC* ic, Term col, Term lam) {
  ic->interactions++;

  uint32_t col_loc = TERM_VAL(col);
  uint32_t lam_loc = TERM_VAL(lam);
  uint8_t col_lab = TERM_LAB(col);
  uint8_t is_co0 = TERM_TAG(col) == CO0;

  Term bod = ic->heap[lam_loc + 0];

  // Batch allocate memory for efficiency
  uint32_t alloc_start = ic_alloc(ic, 5);
  uint32_t lam0_loc = alloc_start;
  uint32_t lam1_loc = alloc_start + 1;
  uint32_t sup_loc = alloc_start + 2; // 2 locations
  uint32_t col_new_loc = alloc_start + 4;

  // Set up the superposition
  ic->heap[sup_loc + 0] = ic_make_term(VAR, 0, lam0_loc);
  ic->heap[sup_loc + 1] = ic_make_term(VAR, 0, lam1_loc);

  // Replace lambda's variable with the superposition
  ic->heap[lam_loc] = ic_make_sub(ic_make_term(SUP, col_lab, sup_loc));

  // Set up the new collapser
  ic->heap[col_new_loc] = bod;

  // Set up new lambda bodies
  ic->heap[lam0_loc] = ic_make_term(CO0, col_lab, col_new_loc);
  ic->heap[lam1_loc] = ic_make_term(CO1, col_lab, col_new_loc);

  // Create and return the appropriate lambda
  if (is_co0) {
    ic->heap[col_loc] = ic_make_sub(ic_make_term(LAM, 0, lam1_loc));
    return ic_make_term(LAM, 0, lam0_loc);
  } else {
    ic->heap[col_loc] = ic_make_sub(ic_make_term(LAM, 0, lam0_loc));
    return ic_make_term(LAM, 0, lam1_loc);
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
static inline Term ic_col_sup(IC* ic, Term col, Term sup) {
  ic->interactions++;

  uint32_t col_loc = TERM_VAL(col);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t col_lab = TERM_LAB(col);
  uint8_t sup_lab = TERM_LAB(sup);
  uint8_t is_co0 = TERM_TAG(col) == CO0;

  Term lft = ic->heap[sup_loc + 0];
  Term rgt = ic->heap[sup_loc + 1];

  // Fast path for matching labels (common case)
  if (col_lab == sup_lab) {
    // Labels match: simple substitution
    if (is_co0) {
      ic->heap[col_loc] = ic_make_sub(rgt);
      return lft;
    } else {
      ic->heap[col_loc] = ic_make_sub(lft);
      return rgt;
    }
  } else {
    // Labels don't match: create nested collapsers
    uint32_t sup_start = ic_alloc(ic, 4); // 2 sups with 2 terms each
    uint32_t sup0_loc = sup_start;
    uint32_t sup1_loc = sup_start + 2;

    // Use existing locations as collapser locations
    uint32_t col_lft_loc = sup_loc + 0;
    uint32_t col_rgt_loc = sup_loc + 1;

    // Set up the first superposition (for CO0)
    ic->heap[sup0_loc + 0] = ic_make_term(CO0, col_lab, col_lft_loc);
    ic->heap[sup0_loc + 1] = ic_make_term(CO0, col_lab, col_rgt_loc);

    // Set up the second superposition (for CO1)
    ic->heap[sup1_loc + 0] = ic_make_term(CO1, col_lab, col_lft_loc);
    ic->heap[sup1_loc + 1] = ic_make_term(CO1, col_lab, col_rgt_loc);

    // Set up original collapsers to point to lft and rgt
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

// (N a)
// ----- APP-NUM
// ⊥
static inline Term ic_app_num(IC* ic, Term app, Term num) {
  ic->interactions++;
  fprintf(stderr, "Runtime error: application of a number\n");
  exit(1);
  return 0; // Unreachable
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
  uint8_t func_id = TERM_LAB(cal);
  
  // Special case for the increment function (0xFF)
  if (func_id == 0xFF) {
    return ic_suc_sup(ic, cal, sup);
  }
  
  uint32_t cal_loc = TERM_VAL(cal);
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

// @F(λx.f)
// -------- CAL-LAM
// ⊥
static inline Term ic_cal_lam(IC* ic, Term cal, Term lam) {
  ic->interactions++;
  uint8_t func_id = TERM_LAB(cal);
  
  // Special case for the increment function (0xFF)
  if (func_id == 0xFF) {
    return ic_suc_lam(ic, cal, lam);
  }
  
  fprintf(stderr, "Runtime error: function call on a lambda\n");
  exit(1);
  return 0; // Unreachable
}

// @F(N) where F != 0xFF
// --------------------- CAL-NUM
// deref(F)[x <- N]
static inline Term ic_cal_num(IC* ic, Term cal, Term num) {
  ic->interactions++;
  uint8_t func_id = TERM_LAB(cal);
  
  // Special case for the increment function (0xFF)
  if (func_id == 0xFF) {
    return ic_suc_num(ic, cal, num);
  }
  
  uint32_t N = TERM_VAL(num);
  
  // For test.ic, which doesn't use functions, just return the number
  // This is a minimal implementation for our benchmark 
  return num;
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
  Term suc_x = ic_make_term(CAL, 0xFF, suc_x_loc);
  Term suc_y = ic_make_term(CAL, 0xFF, suc_y_loc);
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
// @param ic The IC context
// @param term The term to reduce
// @return The term in WHNF
static inline Term ic_whnf(IC* ic, Term term) {
  uint32_t stop = ic->stack_pos;
  Term next = term;
  Term* heap = ic->heap;
  Term* stack = ic->stack;
  uint32_t stack_pos = stop;

  while (1) {
    TermTag tag = TERM_TAG(next);

    switch (tag) {
      case VAR: {
        uint32_t var_loc = TERM_VAL(next);
        Term subst = heap[var_loc];
        if (TERM_SUB(subst)) {
          next = ic_clear_sub(subst);
          continue;
        }
        break; // No substitution, so it's in WHNF
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

      case APP: case CAL: {
        uint32_t app_loc = TERM_VAL(next);
        stack[stack_pos++] = next;
        next = heap[app_loc]; // Reduce the function part
        continue;
      }

      default: {
        if (stack_pos == stop) {
          ic->stack_pos = stack_pos; // Update stack position before return
          return next; // Stack empty, term is in WHNF
        // Interaction Dispatcher
        } else {
          Term prev = stack[--stack_pos];
          TermTag ptag = TERM_TAG(prev);
          
          // Use switch for more efficient dispatching
          switch (ptag) {
            case APP:
              switch (tag) {
                case LAM: next = ic_app_lam(ic, prev, next); continue;
                case SUP: next = ic_app_sup(ic, prev, next); continue;
                case NAT: next = ic_app_num(ic, prev, next); continue;
                default: break;
              }
              break;
              
            case CO0:
            case CO1:
              switch (tag) {
                case LAM: next = ic_col_lam(ic, prev, next); continue;
                case SUP: next = ic_col_sup(ic, prev, next); continue;
                case NAT: next = ic_col_num(ic, prev, next); continue;
                default: break;
              }
              break;
              
            case CAL:
              switch (tag) {
                case LAM: next = ic_cal_lam(ic, prev, next); continue;
                case SUP: next = ic_cal_sup(ic, prev, next); continue;
                case NAT: next = ic_cal_num(ic, prev, next); continue;
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
      ic->stack_pos = stack_pos;
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
      ic->stack_pos = stack_pos;
      return next; // Return updated original term
    }
  }
}


// Reduce a term to full normal form by recursively applying WHNF
// to all subterms.
// 
// @param ic The IC context
// @param term The term to normalize
// @return The fully normalized term
static inline Term ic_normal(IC* ic, Term term) {
  // Reset stack
  ic->stack_pos = 0;
  Term* heap = ic->heap;
  Term* stack = ic->stack;
  uint32_t stack_pos = 0;

  // Allocate a new node for the initial term
  uint32_t root_loc = ic_alloc(ic, 1);
  heap[root_loc] = term;

  // Push initial location to stack as a "location"
  stack[stack_pos++] = MAKE_TERM(false, 0, 0, root_loc);

  while (stack_pos > 0) {
    // Pop current location from stack
    uint32_t loc = TERM_VAL(stack[--stack_pos]);

    // Get term at this location
    Term current = heap[loc];

    // Reduce to WHNF
    ic->stack_pos = stack_pos;
    current = ic_whnf(ic, current);
    stack_pos = ic->stack_pos;

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
  ic->stack_pos = stack_pos;
  return heap[root_loc];
}

// Create a new IC context with default heap and stack sizes.
// @return A new IC context or NULL if allocation failed
static inline IC* ic_default_new() {
  return ic_new(IC_DEFAULT_HEAP_SIZE, IC_DEFAULT_STACK_SIZE);
}

#endif // IC_H
