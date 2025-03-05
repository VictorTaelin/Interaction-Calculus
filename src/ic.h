#ifndef IC_H
#define IC_H

/**
 * Interaction Calculus (IC) - Core header-only implementation
 * 
 * This file contains the full implementation of the Interaction Calculus:
 * - Term representation and bit manipulation
 * - Memory management
 * - Core interactions (app_lam, app_sup, col_lam, col_sup)
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
  APP  // Application
} TermTag;

// Term 32-bit packed representation
typedef uint32_t Term;

// Term components
#define TERM_SUB_MASK 0x80000000UL // 1-bit: Is this a substitution?
#define TERM_TAG_MASK 0x70000000UL // 3-bits: Term tag
#define TERM_LAB_MASK 0x0C000000UL // 2-bits: Label for superpositions
#define TERM_VAL_MASK 0x03FFFFFFUL // 26-bits: Value/pointer

// Term component extraction
#define TERM_SUB(term) (((term) & TERM_SUB_MASK) != 0)
#define TERM_TAG(term) (((term) & TERM_TAG_MASK) >> 28)
#define TERM_LAB(term) (((term) & TERM_LAB_MASK) >> 26)
#define TERM_VAL(term) ((term) & TERM_VAL_MASK)

// Term creation
#define MAKE_TERM(sub, tag, lab, val) \
  (((sub) ? TERM_SUB_MASK : 0) | \
   (((uint32_t)(tag) << 28)) | \
   (((uint32_t)(lab) << 26)) | \
   ((uint32_t)(val) & TERM_VAL_MASK))

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
 * @param lab Label value (for SUP, CO0, CO1)
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

      case APP: {
        uint32_t app_loc = TERM_VAL(next);
        stack[stack_pos++] = next;
        next = heap[app_loc]; // Reduce the function part
        continue;
      }

      default: { // SUP, LAM
        if (stack_pos == stop) {
          ic->stack_pos = stack_pos; // Update stack position before return
          return next; // Stack empty, term is in WHNF
        } else {
          Term prev = stack[--stack_pos];
          TermTag ptag = TERM_TAG(prev);
          
          // Handle interactions based on term types
          if (ptag == APP && tag == LAM) {
            next = ic_app_lam(ic, prev, next);
            continue;
          } 
          else if (ptag == APP && tag == SUP) {
            next = ic_app_sup(ic, prev, next); 
            continue;
          }
          else if ((ptag == CO0 || ptag == CO1) && tag == LAM) {
            next = ic_col_lam(ic, prev, next);
            continue;
          }
          else if ((ptag == CO0 || ptag == CO1) && tag == SUP) {
            next = ic_col_sup(ic, prev, next);
            continue;
          }
          
          // No interaction found, proceed to stack traversal
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
        if (htag == APP || htag == CO0 || htag == CO1) {
          heap[hloc] = next;
        }
        next = host;
      }
      ic->stack_pos = stack_pos;
      return next; // Return updated original term
    }
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
    // Other tags have no subterms to process
  }

  // Update stack position and return the fully normalized term
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
