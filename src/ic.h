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
#define IC_DEFAULT_HEAP_SIZE (1 << 28) // 256M terms (1GB)
#define IC_DEFAULT_STACK_SIZE (1 << 24) // 16M terms
#define IC_DEFAULT_SUBST_SIZE ((1 << 28) / 8) // 32MB (1 bit per term)

// -----------------------------------------------------------------------------
// Core Types and Constants
// -----------------------------------------------------------------------------

// Term tags (expanded to include label in the tag)
typedef enum {
  VAR = 0x0, // Variable
  LAM = 0x1, // Lambda
  APP = 0x2, // Application
  ERA = 0x3, // Erasure (unused)
  SP0 = 0x4, // Superposition with label 0
  SP1 = 0x5, // Superposition with label 1
  SP2 = 0x6, // Superposition with label 2
  SP3 = 0x7, // Superposition with label 3
  CX0 = 0x8, // Collapser first variable with label 0
  CY0 = 0x9, // Collapser second variable with label 0
  CX1 = 0xA, // Collapser first variable with label 1
  CY1 = 0xB, // Collapser second variable with label 1
  CX2 = 0xC, // Collapser first variable with label 2
  CY2 = 0xD, // Collapser second variable with label 2
  CX3 = 0xE, // Collapser first variable with label 3
  CY3 = 0xF, // Collapser second variable with label 3
} TermTag;

// Term 32-bit packed representation
typedef uint32_t Term;

// Term components
#define TERM_TAG_MASK 0xF0000000UL // 4-bits: Term tag
#define TERM_VAL_MASK 0x0FFFFFFFUL // 28-bits: Value/pointer

// Term component extraction
#define TERM_TAG(term) ((TermTag)(((term) & TERM_TAG_MASK) >> 28))
#define TERM_VAL(term) ((term) & TERM_VAL_MASK)

// Helper macros for tag groups
#define IS_SUP(tag) ((tag) >= SP0 && (tag) <= SP3)
#define IS_COL_X(tag) ((tag) >= CX0 && (tag) <= CX3)
#define IS_COL_Y(tag) ((tag) >= CY0 && (tag) <= CY3)
#define IS_COL(tag) (IS_COL_X(tag) || IS_COL_Y(tag))
#define GET_LABEL(tag) (IS_SUP(tag) ? ((tag) - SP0) : (IS_COL(tag) ? (((tag) - CX0) & 0x3) : 0))
#define IS_FIRST_COL(tag) (IS_COL_X(tag))

// Term creation
#define MAKE_TERM(tag, val) \
   (((uint32_t)(tag) << 28) | \
   ((uint32_t)(val) & TERM_VAL_MASK))

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

  // Substitution bitmap (1 bit per term)
  uint8_t* subst;       // Substitution bitmap
  uint32_t subst_size;  // Size of the substitution bitmap in bytes

  // Statistics
  uint64_t interactions; // Interaction counter

} IC;

// Function declarations to avoid circular dependencies
static inline void ic_free(IC* ic);

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
  ic->subst_size = (heap_size + 7) / 8; // Round up to nearest byte

  // Allocate heap, stack, and substitution bitmap
  ic->heap = (Term*)calloc(heap_size, sizeof(Term));
  ic->stack = (Term*)malloc(stack_size * sizeof(Term));
  ic->subst = (uint8_t*)calloc(ic->subst_size, sizeof(uint8_t));

  if (!ic->heap || !ic->stack || !ic->subst) {
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
  if (ic->subst) free(ic->subst);

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
// Substitution Management Functions
// -----------------------------------------------------------------------------

// Set the substitution bit for a location
// @param ic The IC context
// @param loc The location to mark as a substitution
static inline void ic_set_subst(IC* ic, uint32_t loc) {
  if (loc < ic->heap_size) {
    uint32_t byte_idx = loc / 8;
    uint8_t bit_idx = loc % 8;
    ic->subst[byte_idx] |= (1 << bit_idx);
  }
}

// Check if a location is marked as a substitution
// @param ic The IC context
// @param loc The location to check
// @return true if the location is marked as a substitution, false otherwise
static inline bool ic_is_subst(IC* ic, uint32_t loc) {
  if (loc < ic->heap_size) {
    uint32_t byte_idx = loc / 8;
    uint8_t bit_idx = loc % 8;
    return (ic->subst[byte_idx] & (1 << bit_idx)) != 0;
  }
  return false;
}

// Clear the substitution bit for a location
// @param ic The IC context
// @param loc The location to clear the substitution bit for
static inline void ic_clear_subst(IC* ic, uint32_t loc) {
  if (loc < ic->heap_size) {
    uint32_t byte_idx = loc / 8;
    uint8_t bit_idx = loc % 8;
    ic->subst[byte_idx] &= ~(1 << bit_idx);
  }
}

// -----------------------------------------------------------------------------
// Term Manipulation Functions
// -----------------------------------------------------------------------------

// Create a term with the given tag and value.
// @param tag Term type tag
// @param val Value/pointer into the heap
// @return The constructed term
static inline Term ic_make_term(TermTag tag, uint32_t val) {
  return MAKE_TERM(tag, val);
}

// Create a superposition term with the given label.
// @param label Label value (0-3)
// @param val Value/pointer into the heap
// @return The constructed superposition term
static inline Term ic_make_sup(uint8_t label, uint32_t val) {
  return MAKE_TERM((TermTag)(SP0 + (label & 0x3)), val);
}

// Create a collapser variable (first) with the given label.
// @param label Label value (0-3)
// @param val Value/pointer into the heap
// @return The constructed collapser term
static inline Term ic_make_col_x(uint8_t label, uint32_t val) {
  return MAKE_TERM((TermTag)(CX0 + (label & 0x3)), val);
}

// Create a collapser variable (second) with the given label.
// @param label Label value (0-3)
// @param val Value/pointer into the heap
// @return The constructed collapser term
static inline Term ic_make_col_y(uint8_t label, uint32_t val) {
  return MAKE_TERM((TermTag)(CY0 + (label & 0x3)), val);
}

// Get the label from a term tag
// @param tag Term tag
// @return The label value (0-3)
static inline uint8_t ic_get_label(TermTag tag) {
  if (IS_SUP(tag)) {
    return tag - SP0;
  } else if (IS_COL(tag)) {
    return (tag - CX0) & 0x3;
  }
  return 0;
}

// Check if a term tag is a collapser variable (first or second)
// @param tag Term tag
// @return true if the tag is a collapser variable, false otherwise
static inline bool ic_is_col(TermTag tag) {
  return (tag >= CX0 && tag <= CY3);
}

// Check if a term tag is a first collapser variable
// @param tag Term tag
// @return true if the tag is a first collapser variable, false otherwise
static inline bool ic_is_col_x(TermTag tag) {
  return (tag >= CX0 && tag <= CX3);
}

// Check if a term tag is a second collapser variable
// @param tag Term tag
// @return true if the tag is a second collapser variable, false otherwise
static inline bool ic_is_col_y(TermTag tag) {
  return (tag >= CY0 && tag <= CY3);
}

// Check if a term tag is a superposition
// @param tag Term tag
// @return true if the tag is a superposition, false otherwise
static inline bool ic_is_sup(TermTag tag) {
  return (tag >= SP0 && tag <= SP3);
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
  ic->heap[lam_loc] = arg;
  ic_set_subst(ic, lam_loc);

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
  TermTag sup_tag = TERM_TAG(sup);
  uint8_t sup_lab = ic_get_label(sup_tag);

  Term arg = ic->heap[app_loc + 1];
  Term lft = ic->heap[sup_loc + 0];
  Term rgt = ic->heap[sup_loc + 1];

  // Allocate only what's necessary
  uint32_t col_loc = ic_alloc(ic, 1);
  uint32_t app1_loc = ic_alloc(ic, 2);

  // Store the arg in the collapser location
  ic->heap[col_loc] = arg;

  // Create collapser terms
  Term x0 = ic_make_col_x(sup_lab, col_loc);
  Term x1 = ic_make_col_y(sup_lab, col_loc);

  // Reuse sup_loc for app0
  ic->heap[sup_loc + 1] = x0; // lft is already in heap[sup_loc + 0]

  // Set up app1
  ic->heap[app1_loc + 0] = rgt;
  ic->heap[app1_loc + 1] = x1;

  // Reuse app_loc for the result superposition
  ic->heap[app_loc + 0] = ic_make_term(APP, sup_loc);
  ic->heap[app_loc + 1] = ic_make_term(APP, app1_loc);

  return ic_make_sup(sup_lab, app_loc);
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
  TermTag col_tag = TERM_TAG(col);
  uint8_t col_lab = ic_get_label(col_tag);
  bool is_col_x = ic_is_col_x(col_tag);

  Term bod = ic->heap[lam_loc + 0];

  // Batch allocate memory for efficiency
  uint32_t alloc_start = ic_alloc(ic, 5);
  uint32_t lam0_loc = alloc_start;
  uint32_t lam1_loc = alloc_start + 1;
  uint32_t sup_loc = alloc_start + 2; // 2 locations
  uint32_t col_new_loc = alloc_start + 4;

  // Set up the superposition
  ic->heap[sup_loc + 0] = ic_make_term(VAR, lam0_loc);
  ic->heap[sup_loc + 1] = ic_make_term(VAR, lam1_loc);

  // Replace lambda's variable with the superposition
  ic->heap[lam_loc] = ic_make_sup(col_lab, sup_loc);
  ic_set_subst(ic, lam_loc);

  // Set up the new collapser
  ic->heap[col_new_loc] = bod;

  // Set up new lambda bodies
  ic->heap[lam0_loc] = ic_make_col_x(col_lab, col_new_loc);
  ic->heap[lam1_loc] = ic_make_col_y(col_lab, col_new_loc);

  // Create and return the appropriate lambda
  if (is_col_x) {
    ic->heap[col_loc] = ic_make_term(LAM, lam1_loc);
    ic_set_subst(ic, col_loc);
    return ic_make_term(LAM, lam0_loc);
  } else {
    ic->heap[col_loc] = ic_make_term(LAM, lam0_loc);
    ic_set_subst(ic, col_loc);
    return ic_make_term(LAM, lam1_loc);
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
  TermTag col_tag = TERM_TAG(col);
  TermTag sup_tag = TERM_TAG(sup);
  uint8_t col_lab = ic_get_label(col_tag);
  uint8_t sup_lab = ic_get_label(sup_tag);
  bool is_col_x = ic_is_col_x(col_tag);

  Term lft = ic->heap[sup_loc + 0];
  Term rgt = ic->heap[sup_loc + 1];

  // Fast path for matching labels (common case)
  if (col_lab == sup_lab) {
    // Labels match: simple substitution
    if (is_col_x) {
      ic->heap[col_loc] = rgt;
      ic_set_subst(ic, col_loc);
      return lft;
    } else {
      ic->heap[col_loc] = lft;
      ic_set_subst(ic, col_loc);
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
    ic->heap[sup0_loc + 0] = ic_make_col_x(col_lab, col_lft_loc);
    ic->heap[sup0_loc + 1] = ic_make_col_x(col_lab, col_rgt_loc);

    // Set up the second superposition (for CO1)
    ic->heap[sup1_loc + 0] = ic_make_col_y(col_lab, col_lft_loc);
    ic->heap[sup1_loc + 1] = ic_make_col_y(col_lab, col_rgt_loc);

    // Set up original collapsers to point to lft and rgt
    ic->heap[col_lft_loc] = lft;
    ic->heap[col_rgt_loc] = rgt;

    if (is_col_x) {
      ic->heap[col_loc] = ic_make_sup(sup_lab, sup1_loc);
      ic_set_subst(ic, col_loc);
      return ic_make_sup(sup_lab, sup0_loc);
    } else {
      ic->heap[col_loc] = ic_make_sup(sup_lab, sup0_loc);
      ic_set_subst(ic, col_loc);
      return ic_make_sup(sup_lab, sup1_loc);
    }
  }
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
        if (ic_is_subst(ic, var_loc)) {
          next = heap[var_loc];
          continue;
        }
        break; // No substitution, so it's in WHNF
      }

      case CX0:
      case CY0:
      case CX1:
      case CY1:
      case CX2:
      case CY2:
      case CX3:
      case CY3: {
        uint32_t col_loc = TERM_VAL(next);
        if (ic_is_subst(ic, col_loc)) {
          next = heap[col_loc];
          continue;
        } else {
          stack[stack_pos++] = next;
          next = heap[col_loc];
          continue;
        }
      }

      case APP: {
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
                case SP0:
                case SP1:
                case SP2:
                case SP3: next = ic_app_sup(ic, prev, next); continue;
                default: break;
              }
              break;

            case CX0:
            case CY0:
            case CX1:
            case CY1:
            case CX2:
            case CY2:
            case CX3:
            case CY3:
              switch (tag) {
                case LAM: next = ic_col_lam(ic, prev, next); continue;
                case SP0:
                case SP1:
                case SP2:
                case SP3: next = ic_col_sup(ic, prev, next); continue;
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
        if (htag == APP || ic_is_col(htag)) {
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
  stack[stack_pos++] = MAKE_TERM(VAR, root_loc);

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
      stack[stack_pos++] = MAKE_TERM(VAR, val);
    }
    else if (tag == APP || ic_is_sup(tag)) {
      // Both APP and SUP need to push two locations
      stack[stack_pos++] = MAKE_TERM(VAR, val);
      stack[stack_pos++] = MAKE_TERM(VAR, val + 1);
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
