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
 * 
 * The code is structured to be compatible with both standard C and CUDA.
 */

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

// Default heap and stack sizes
#define IC_DEFAULT_HEAP_SIZE (1 << 28) // 256M terms
#define IC_DEFAULT_STACK_SIZE (1 << 24) // 16M terms

// CUDA compatibility macros
#ifdef __CUDACC__
#define IC_DEVICE __device__
#else
#define IC_DEVICE
#endif

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
 APP // Application
} TermTag;

// Term 32-bit packed representation
typedef uint32_t Term;

// Term components
#define TERM_SUB_MASK 0x80000000UL // 1-bit: Is this a substitution?
#define TERM_TAG_MASK 0x70000000UL // 3-bits: Term tag
#define TERM_LAB_MASK 0x0C000000UL // 2-bits: Label for superpositions
#define TERM_VAL_MASK 0x03FFFFFFUL // 26-bits: Value/pointer

// Term manipulation macros
#define TERM_SUB(term) (((term) & TERM_SUB_MASK) != 0)
#define TERM_TAG(term) (((term) & TERM_TAG_MASK) >> 28)
#define TERM_LAB(term) (((term) & TERM_LAB_MASK) >> 26)
#define TERM_VAL(term) ((term) & TERM_VAL_MASK)

// Term creation macro
#define MAKE_TERM(sub, tag, lab, val) \
 (((sub) ? TERM_SUB_MASK : 0) | \
  (((uint32_t)(tag) << 28) & TERM_TAG_MASK) | \
  (((uint32_t)(lab) << 26) & TERM_LAB_MASK) | \
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
  
  // Allocate heap
  ic->heap = (Term*)malloc(heap_size * sizeof(Term));
  if (!ic->heap) {
    free(ic);
    return NULL;
  }
  
  // Allocate stack
  ic->stack = (Term*)malloc(stack_size * sizeof(Term));
  if (!ic->stack) {
    free(ic->heap);
    free(ic);
    return NULL;
  }
  
  // Clear memory
  for (uint32_t i = 0; i < heap_size; i++) {
    ic->heap[i] = 0;
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
  return MAKE_TERM(true, TERM_TAG(term), TERM_LAB(term), TERM_VAL(term));
}

/**
 * Remove the substitution bit from a term.
 * @param term The term to clear the substitution bit from
 * @return The term with its substitution bit cleared
 */
static inline Term ic_clear_sub(Term term) {
  return MAKE_TERM(false, TERM_TAG(term), TERM_LAB(term), TERM_VAL(term));
}

/**
 * Check if a term is a substitution.
 * @param term The term to check
 * @return true if the term is a substitution, false otherwise
 */
static inline bool ic_has_sub(Term term) {
  return TERM_SUB(term);
}

// -----------------------------------------------------------------------------
// Stack Operations
// -----------------------------------------------------------------------------

/**
 * Push a term onto the evaluation stack.
 * @param ic The IC context
 * @param term The term to push
 */
static inline void ic_stack_push(IC* ic, Term term) {
  if (ic->stack_pos >= ic->stack_size) {
    fprintf(stderr, "Stack overflow in term evaluation\n");
    exit(1);
  }
  ic->stack[ic->stack_pos++] = term;
}

/**
 * Pop a term from the evaluation stack.
 * @param ic The IC context
 * @return The popped term
 */
static inline Term ic_stack_pop(IC* ic) {
  if (ic->stack_pos == 0) {
    fprintf(stderr, "Stack underflow in term evaluation\n");
    exit(1);
  }
  return ic->stack[--ic->stack_pos];
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
  uint32_t arg_loc = app_loc + 1;

  Term arg = ic->heap[arg_loc + 0];
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

  uint32_t col_loc = ic_alloc(ic, 1);
  ic->heap[col_loc] = arg;

  Term x0 = ic_make_term(CO0, sup_lab, col_loc);
  Term x1 = ic_make_term(CO1, sup_lab, col_loc);

  // Instead of allocating new memory for app0, reuse sup_loc
  uint32_t app0_loc = sup_loc;
  // lft is already in heap[app0_loc + 0]
  ic->heap[app0_loc + 1] = x0;

  uint32_t app1_loc = ic_alloc(ic, 2);
  ic->heap[app1_loc + 0] = rgt;
  ic->heap[app1_loc + 1] = x1;

  // Instead of allocating new memory for sup_new, reuse app_loc
  uint32_t sup_new_loc = app_loc;
  ic->heap[sup_new_loc + 0] = ic_make_term(APP, 0, app0_loc);
  ic->heap[sup_new_loc + 1] = ic_make_term(APP, 0, app1_loc);

  return ic_make_term(SUP, sup_lab, sup_new_loc);
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

  // Create new lambda variables
  uint32_t lam0_loc = ic_alloc(ic, 1);
  uint32_t lam1_loc = ic_alloc(ic, 1);

  // Create a superposition for lambda's variable
  uint32_t sup_loc = ic_alloc(ic, 2);
  ic->heap[sup_loc + 0] = ic_make_term(VAR, 0, lam0_loc);
  ic->heap[sup_loc + 1] = ic_make_term(VAR, 0, lam1_loc);

  // Replace lambda's variable with the superposition
  ic->heap[lam_loc] = ic_make_sub(ic_make_term(SUP, col_lab, sup_loc));

  // Create a collapser for lambda's body
  uint32_t col_new_loc = ic_alloc(ic, 1);
  ic->heap[col_new_loc] = bod;

  // Set up new lambda bodies
  ic->heap[lam0_loc] = ic_make_term(CO0, col_lab, col_new_loc);
  ic->heap[lam1_loc] = ic_make_term(CO1, col_lab, col_new_loc);

  // Create and return the appropriate lambda
  Term new_lam;
  if (is_co0) {
    ic->heap[col_loc] = ic_make_sub(ic_make_term(LAM, 0, lam1_loc));
    new_lam = ic_make_term(LAM, 0, lam0_loc);
  } else {
    ic->heap[col_loc] = ic_make_sub(ic_make_term(LAM, 0, lam0_loc));
    new_lam = ic_make_term(LAM, 0, lam1_loc);
  }

  return new_lam;
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
    uint32_t col_lft_loc = sup_loc + 0; // Reuse memory locations to avoid alloc
    uint32_t col_rgt_loc = sup_loc + 1; // lft/rgt already at these locations

    uint32_t sup0_loc = ic_alloc(ic, 2);
    ic->heap[sup0_loc + 0] = ic_make_term(CO0, col_lab, col_lft_loc);
    ic->heap[sup0_loc + 1] = ic_make_term(CO0, col_lab, col_rgt_loc);

    uint32_t sup1_loc = ic_alloc(ic, 2);
    ic->heap[sup1_loc + 0] = ic_make_term(CO1, col_lab, col_lft_loc);
    ic->heap[sup1_loc + 1] = ic_make_term(CO1, col_lab, col_rgt_loc);

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

  while (1) {
    TermTag tag = TERM_TAG(next);

    switch (tag) {
      case VAR: {
        uint32_t var_loc = TERM_VAL(next);
        Term subst = ic->heap[var_loc];
        if (TERM_SUB(subst)) {
          next = ic_clear_sub(subst);
          continue;
        }
        break; // No substitution, so it's in WHNF
      }

      case CO0:
      case CO1: {
        uint32_t col_loc = TERM_VAL(next);
        Term val = ic->heap[col_loc];
        if (TERM_SUB(val)) {
          next = ic_clear_sub(val);
          continue;
        } else {
          ic_stack_push(ic, next);
          next = val;
          continue;
        }
      }

      case APP: {
        uint32_t app_loc = TERM_VAL(next);
        ic_stack_push(ic, next);
        next = ic->heap[app_loc]; // Reduce the function part
        continue;
      }

      default: { // SUP, LAM
        if (ic->stack_pos == stop) {
          return next; // Stack empty, term is in WHNF
        } else {
          Term prev = ic_stack_pop(ic);
          TermTag ptag = TERM_TAG(prev);
          
          switch (ptag) {
            case APP: {
              switch (tag) {
                case LAM: next = ic_app_lam(ic, prev, next); continue;
                case SUP: next = ic_app_sup(ic, prev, next); continue;
              }
              break;
            }
            case CO0:
            case CO1: {
              switch (tag) {
                case LAM: next = ic_col_lam(ic, prev, next); continue;
                case SUP: next = ic_col_sup(ic, prev, next); continue;
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
    if (ic->stack_pos == stop) {
      return next; // Stack empty, return WHNF
    } else {
      while (ic->stack_pos > stop) {
        Term host = ic_stack_pop(ic);
        TermTag htag = TERM_TAG(host);
        uint32_t hloc = TERM_VAL(host);
        
        switch (htag) {
          case APP: ic->heap[hloc] = next; break;
          case CO0:
          case CO1: ic->heap[hloc] = next; break;
        }
        next = host;
      }
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

  // Allocate a new node for the initial term
  uint32_t root_loc = ic_alloc(ic, 1);
  ic->heap[root_loc] = term;

  // Push initial location to stack as a "location"
  // We're using the same stack but with a different meaning for values
  ic_stack_push(ic, ic_make_term(0, 0, root_loc)); // Not a real term, just using it to store location

  while (ic->stack_pos > 0) {
    // Pop current location from stack
    uint32_t loc = TERM_VAL(ic_stack_pop(ic));

    // Get term at this location
    Term current = ic->heap[loc];

    // Reduce to WHNF
    current = ic_whnf(ic, current);

    // Store the WHNF term back to the heap
    ic->heap[loc] = current;

    // Get term details
    TermTag tag = TERM_TAG(current);
    uint32_t val = TERM_VAL(current);

    // Push subterm locations based on term type
    switch (tag) {
      case LAM:
        ic_stack_push(ic, ic_make_term(0, 0, val)); // Push body location
        break;
      case APP:
        ic_stack_push(ic, ic_make_term(0, 0, val));   // Push function location
        ic_stack_push(ic, ic_make_term(0, 0, val + 1)); // Push argument location
        break;
      case SUP:
        ic_stack_push(ic, ic_make_term(0, 0, val));   // Push left location
        ic_stack_push(ic, ic_make_term(0, 0, val + 1)); // Push right location
        break;
      default:
        // No subterms to process for VAR, CO0, CO1
        break;
    }
  }

  // Get the fully normalized term
  return ic->heap[root_loc];
}

/**
 * Create a new IC context with default heap and stack sizes.
 * @return A new IC context or NULL if allocation failed
 */
static inline IC* ic_default_new() {
  return ic_new(IC_DEFAULT_HEAP_SIZE, IC_DEFAULT_STACK_SIZE);
}

/**
 * Copy a term and its subterms from a source heap to the IC heap.
 * This is used when importing terms from the parser into the IC context.
 * 
 * @param ic The IC context
 * @param term The term to copy
 * @param src_heap The source heap containing the term
 * @param src_heap_size The size of the source heap (number of used terms)
 * @return The copied term in the IC heap
 */
static inline Term ic_copy_term(IC* ic, Term term, Term* src_heap, uint32_t src_heap_size) {
  // Create a mapping from source locations to target locations
  // We'll use a limited map that can only handle src_heap_size entries
  uint32_t* loc_map = (uint32_t*)calloc(src_heap_size + 1, sizeof(uint32_t));
  if (!loc_map) {
    fprintf(stderr, "Error: Failed to allocate location map\n");
    exit(1);
  }
  
  // Stack for tracking terms to copy
  uint32_t copy_stack[IC_DEFAULT_STACK_SIZE];
  uint32_t stack_pos = 0;
  
  // Initialize with the root term
  copy_stack[stack_pos++] = TERM_VAL(term);
  uint32_t root_loc = ic_alloc(ic, 1);
  loc_map[TERM_VAL(term)] = root_loc;
  
  // Process each term in the stack
  while (stack_pos > 0) {
    uint32_t src_loc = copy_stack[--stack_pos];
    uint32_t dst_loc = loc_map[src_loc];
    
    // Get source term
    Term src_term = src_heap[src_loc];
    TermTag tag = TERM_TAG(src_term);
    
    // Process based on term type
    switch (tag) {
      case LAM: {
        // Allocate space for the lambda body
        uint32_t body_loc = ic_alloc(ic, 1);
        
        // Get source location of the body (which is just the next element)
        uint32_t src_body_loc = src_loc;
        
        // Add body to copy queue
        copy_stack[stack_pos++] = src_body_loc;
        loc_map[src_body_loc] = body_loc;
        
        // Store LAM term pointing to the allocated space
        ic->heap[dst_loc] = ic_make_term(LAM, TERM_LAB(src_term), body_loc);
        break;
      }
      
      case APP: {
        // Allocate space for function and argument
        uint32_t app_loc = ic_alloc(ic, 2);

        // Get source locations
        uint32_t src_fun_loc = src_loc;
        uint32_t src_arg_loc = src_loc + 1;
        
        // Add function and argument to copy queue
        copy_stack[stack_pos++] = src_fun_loc;
        copy_stack[stack_pos++] = src_arg_loc;
        loc_map[src_fun_loc] = app_loc;
        loc_map[src_arg_loc] = app_loc + 1;
        
        // Store APP term pointing to the allocated space
        ic->heap[dst_loc] = ic_make_term(APP, TERM_LAB(src_term), app_loc);
        break;
      }
      
      case SUP: {
        // Allocate space for left and right terms
        uint32_t sup_loc = ic_alloc(ic, 2);

        // Get source locations
        uint32_t src_lft_loc = src_loc;
        uint32_t src_rgt_loc = src_loc + 1;
        
        // Add left and right to copy queue
        copy_stack[stack_pos++] = src_lft_loc;
        copy_stack[stack_pos++] = src_rgt_loc;
        loc_map[src_lft_loc] = sup_loc;
        loc_map[src_rgt_loc] = sup_loc + 1;
        
        // Store SUP term pointing to the allocated space
        ic->heap[dst_loc] = ic_make_term(SUP, TERM_LAB(src_term), sup_loc);
        break;
      }
      
      case VAR:
      case CO0:
      case CO1: {
        // These are simple pointers, just copy
        ic->heap[dst_loc] = src_term;
        break;
      }
    }
  }
  
  // Create the result term
  Term result = ic_make_term(TERM_TAG(term), TERM_LAB(term), root_loc);
  
  // Clean up
  free(loc_map);
  
  return result;
}

// These constants are defined at the top of the file
// IC_DEFAULT_HEAP_SIZE and IC_DEFAULT_STACK_SIZE

#endif // IC_H
