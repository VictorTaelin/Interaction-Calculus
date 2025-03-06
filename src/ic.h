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
#define IC_DEFAULT_HEAP_SIZE (1 << 24) // 16M terms
#define IC_DEFAULT_STACK_SIZE (1 << 24) // 16M terms

// -----------------------------------------------------------------------------
// Core Types and Constants
// -----------------------------------------------------------------------------

// Term tags for new, compact memory format
typedef enum {
  VAR = 0x0, // Variable
  LAM = 0x1, // Lambda
  APP = 0x2, // Application
  ERA = 0x3, // Erasure (not used yet)
  SP0 = 0x4, // Superposition with label 0
  SP1 = 0x5, // Superposition with label 1
  SP2 = 0x6, // Superposition with label 2
  SP3 = 0x7, // Superposition with label 3
  CX0 = 0x8, // Collapser first variable with label 0
  CX1 = 0x9, // Collapser first variable with label 1
  CX2 = 0xA, // Collapser first variable with label 2
  CX3 = 0xB, // Collapser first variable with label 3
  CY0 = 0xC, // Collapser second variable with label 0
  CY1 = 0xD, // Collapser second variable with label 1
  CY2 = 0xE, // Collapser second variable with label 2
  CY3 = 0xF, // Collapser second variable with label 3
} TermTag;

// Term 32-bit packed representation
typedef uint32_t Term;

// Term components
#define TERM_SUB_MASK 0x80000000UL // 1-bit: Is this a substitution?
#define TERM_TAG_MASK 0x78000000UL // 4-bits: Term tag
#define TERM_VAL_MASK 0x07FFFFFFUL // 27-bits: Value/pointer

// Term component extraction
#define TERM_SUB(term) (((term) & TERM_SUB_MASK) != 0)
#define TERM_TAG(term) ((TermTag)(((term) & TERM_TAG_MASK) >> 27))
#define TERM_VAL(term) ((term) & TERM_VAL_MASK)

// Label helpers (for compatibility with existing code)
#define TERM_LAB(term) ((TERM_TAG(term) & 0x3)) // Extract label from tag (last 2 bits)
#define IS_SUP(tag) ((tag) >= SP0 && (tag) <= SP3)
#define IS_CO0(tag) ((tag) >= CX0 && (tag) <= CX3)
#define IS_CO1(tag) ((tag) >= CY0 && (tag) <= CY3)
// Check if tag is a collapser (CO0 or CO1)
#define IS_COL(tag) ((tag) >= CX0 && (tag) <= CY3)
#define SUP_TAG(lab) ((TermTag)(SP0 + ((lab) & 0x3)))
#define CO0_TAG(lab) ((TermTag)(CX0 + ((lab) & 0x3)))
#define CO1_TAG(lab) ((TermTag)(CY0 + ((lab) & 0x3)))

// Term creation
#define MAKE_TERM(sub, tag, val) \
  (((sub) ? TERM_SUB_MASK : 0) | \
   (((uint32_t)(tag) << 27)) | \
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

  // Allocate heap and stack - use malloc instead of calloc for better performance
  // Only initialize the part of memory we need immediately
  ic->heap = (Term*)malloc(heap_size * sizeof(Term));
  ic->stack = (Term*)malloc(stack_size * sizeof(Term));

  if (!ic->heap || !ic->stack) {
    ic_free(ic);
    return NULL;
  }
  
  // Only zero out a small initial portion of the heap
  // (Most heap cells will be written before reading anyway)
  memset(ic->heap, 0, 4096 * sizeof(Term));

  return ic;
}

// Free all resources associated with an IC context.
// @param ic The IC context to free
static inline void ic_free(IC* ic) {
  if (!ic) return;

  if (ic->heap) free(ic->heap);
  if (ic->stack) free(ic->stack);

  free(ic);
}

// Allocate n consecutive terms in memory.
// @param ic The IC context
// @param n Number of terms to allocate
// @return Location in the heap
// Fast heap allocation with batching
// Uses bump allocation strategy for maximum performance
#define IC_ALLOC_BATCH_SIZE 1024  // Batch size for allocation checks
static inline uint32_t ic_alloc(IC* ic, uint32_t n) {
  // One check for every BATCH_SIZE allocations for better performance
  static uint32_t alloc_counter = 0;  
  
  uint32_t ptr = ic->heap_pos;
  ic->heap_pos += n;
  
  // Only check heap bounds periodically or for large allocations
  if (++alloc_counter >= IC_ALLOC_BATCH_SIZE || n > 16) {
    alloc_counter = 0;
    if (ic->heap_pos >= ic->heap_size) {
      fprintf(stderr, "Error: Out of memory (tried to allocate %u terms, %u/%u used)\n", 
              n, ic->heap_pos, ic->heap_size);
      exit(1);
    }
  }
  
  return ptr;
}

// -----------------------------------------------------------------------------
// Term Manipulation Functions
// -----------------------------------------------------------------------------

// Create a term with the given tag and value.
// @param tag Term type tag (includes label for SUP, CX, CY)
// @param val Value/pointer into the heap
// @return The constructed term
static inline Term ic_make_term(TermTag tag, uint32_t val) {
  return MAKE_TERM(false, tag, val);
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

// Helper to create a term with the appropriate superposition tag for a label
// @param lab Label value (0-3)
// @param val Value/pointer into the heap
// @return The constructed superposition term
static inline Term ic_make_sup(uint8_t lab, uint32_t val) {
  return ic_make_term(SUP_TAG(lab), val);
}

// Helper to create a CO0 term with the appropriate tag for a label
// @param lab Label value (0-3)
// @param val Value/pointer into the heap
// @return The constructed CO0 term
static inline Term ic_make_co0(uint8_t lab, uint32_t val) {
  return ic_make_term(CO0_TAG(lab), val);
}

// Helper to create a CO1 term with the appropriate tag for a label
// @param lab Label value (0-3)
// @param val Value/pointer into the heap
// @return The constructed CO1 term
static inline Term ic_make_co1(uint8_t lab, uint32_t val) {
  return ic_make_term(CO1_TAG(lab), val);
}

// -----------------------------------------------------------------------------
// Interaction Functions
// -----------------------------------------------------------------------------

//(λx.f a)
//-------- APP-LAM
//x <- a
//f
// Optimized lambda application interaction
static inline Term ic_app_lam(IC* ic, Term app, Term lam) {
  ic->interactions++;

  // Fast access to heap 
  Term* heap = ic->heap;
  uint32_t app_loc = TERM_VAL(app);
  uint32_t lam_loc = TERM_VAL(lam);

  // Direct access to argument and body
  Term arg = heap[app_loc + 1];
  Term bod = heap[lam_loc];

  // Create substitution directly without function call
  heap[lam_loc] = arg | TERM_SUB_MASK;

  return bod;
}

//(&L{a,b} c)
//----------------- APP-SUP
//! &L{c0,c1} = c;
//&L{(a c0),(b c1)}
// Optimized superposition application interaction
static inline Term ic_app_sup(IC* ic, Term app, Term sup) {
  ic->interactions++;

  // Direct heap access
  Term* heap = ic->heap;
  uint32_t app_loc = TERM_VAL(app);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t sup_lab = TERM_LAB(sup);
  TermTag sup_tag = TERM_TAG(sup);

  // Get argument and superposition parts
  Term arg = heap[app_loc + 1];
  Term lft = heap[sup_loc + 0];
  Term rgt = heap[sup_loc + 1];

  // Fast allocation without function call overhead
  uint32_t col_loc = ic->heap_pos;
  uint32_t app1_loc = col_loc + 1; 
  ic->heap_pos += 3; // col_loc (1) + app1_loc (2)

  // Store arg in collapser location
  heap[col_loc] = arg;

  // Create CO0 and CO1 terms directly
  Term x0 = MAKE_TERM(false, CO0_TAG(sup_lab), col_loc);
  Term x1 = MAKE_TERM(false, CO1_TAG(sup_lab), col_loc);

  // Reuse sup_loc for app0 (lft is already there)
  heap[sup_loc + 1] = x0;

  // Set up app1
  heap[app1_loc + 0] = rgt;
  heap[app1_loc + 1] = x1;

  // Reuse app_loc for result superposition (direct term creation)
  heap[app_loc + 0] = MAKE_TERM(false, APP, sup_loc);
  heap[app_loc + 1] = MAKE_TERM(false, APP, app1_loc);

  // Return with direct term creation
  return MAKE_TERM(false, sup_tag, app_loc);
}

//! &L{r,s} = λx.f;
//K
//----------------- COL-LAM
//r <- λx0.f0
//s <- λx1.f1
//x <- &L{x0,x1}
//! &L{f0,f1} = f;
//K
// Optimized collapser-lambda interaction
static inline Term ic_col_lam(IC* ic, Term col, Term lam) {
  ic->interactions++;

  // Direct heap access
  Term* heap = ic->heap;
  uint32_t col_loc = TERM_VAL(col);
  uint32_t lam_loc = TERM_VAL(lam);
  uint8_t col_lab = TERM_LAB(col);
  TermTag col_tag = TERM_TAG(col);
  uint8_t is_co0 = IS_CO0(col_tag);

  // Get lambda's body
  Term bod = heap[lam_loc];

  // Fast batch allocation
  uint32_t alloc_start = ic->heap_pos;
  ic->heap_pos += 5; // Allocate 5 locations in one go
  
  uint32_t lam0_loc = alloc_start;
  uint32_t lam1_loc = alloc_start + 1;
  uint32_t sup_loc = alloc_start + 2; // 2 locations
  uint32_t col_new_loc = alloc_start + 4;

  // Set up the superposition with direct term creation
  heap[sup_loc + 0] = MAKE_TERM(false, VAR, lam0_loc);
  heap[sup_loc + 1] = MAKE_TERM(false, VAR, lam1_loc);

  // Replace lambda's variable with the superposition
  // Use direct term creation
  heap[lam_loc] = MAKE_TERM(true, SUP_TAG(col_lab), sup_loc);

  // Set up the new collapser
  heap[col_new_loc] = bod;

  // Set up new lambda bodies with direct term creation
  heap[lam0_loc] = MAKE_TERM(false, CO0_TAG(col_lab), col_new_loc);
  heap[lam1_loc] = MAKE_TERM(false, CO1_TAG(col_lab), col_new_loc);

  // Create substitution and return lambda - avoid function calls
  if (is_co0) {
    heap[col_loc] = MAKE_TERM(true, LAM, lam1_loc);
    return MAKE_TERM(false, LAM, lam0_loc);
  } else {
    heap[col_loc] = MAKE_TERM(true, LAM, lam0_loc);
    return MAKE_TERM(false, LAM, lam1_loc);
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
// Optimized collapser-superposition interaction 
static inline Term ic_col_sup(IC* ic, Term col, Term sup) {
  ic->interactions++;

  // Direct heap access
  Term* heap = ic->heap;
  uint32_t col_loc = TERM_VAL(col);
  uint32_t sup_loc = TERM_VAL(sup);
  uint8_t col_lab = TERM_LAB(col);
  uint8_t sup_lab = TERM_LAB(sup);
  TermTag col_tag = TERM_TAG(col);
  TermTag sup_tag = TERM_TAG(sup);
  uint8_t is_co0 = IS_CO0(col_tag);

  // Get left and right parts of superposition
  Term lft = heap[sup_loc + 0];
  Term rgt = heap[sup_loc + 1];

  // Branch prediction hint - labels usually match
  if (__builtin_expect(col_lab == sup_lab, 1)) {
    // Fast path: labels match - simple substitution
    if (is_co0) {
      // Direct substitution creation
      heap[col_loc] = rgt | TERM_SUB_MASK;
      return lft;
    } else {
      heap[col_loc] = lft | TERM_SUB_MASK;
      return rgt;
    }
  } else {
    // Slow path: labels don't match - create nested collapsers
    // Fast allocation
    uint32_t sup_start = ic->heap_pos;
    ic->heap_pos += 4; // Allocate 4 locations (2 sups with 2 terms each)
    
    uint32_t sup0_loc = sup_start;
    uint32_t sup1_loc = sup_start + 2;

    // Use existing locations as collapser locations
    uint32_t col_lft_loc = sup_loc + 0; 
    uint32_t col_rgt_loc = sup_loc + 1;

    // Set up superpositions with direct term creation
    // First superposition (for CO0)
    heap[sup0_loc + 0] = MAKE_TERM(false, CO0_TAG(col_lab), col_lft_loc);
    heap[sup0_loc + 1] = MAKE_TERM(false, CO0_TAG(col_lab), col_rgt_loc);

    // Second superposition (for CO1)
    heap[sup1_loc + 0] = MAKE_TERM(false, CO1_TAG(col_lab), col_lft_loc);
    heap[sup1_loc + 1] = MAKE_TERM(false, CO1_TAG(col_lab), col_rgt_loc);

    // Set up original collapsers to point to lft and rgt
    heap[col_lft_loc] = lft;
    heap[col_rgt_loc] = rgt;

    // Return the appropriate superposition
    if (is_co0) {
      heap[col_loc] = MAKE_TERM(true, SUP_TAG(sup_lab), sup1_loc);
      return MAKE_TERM(false, SUP_TAG(sup_lab), sup0_loc);
    } else {
      heap[col_loc] = MAKE_TERM(true, SUP_TAG(sup_lab), sup0_loc);
      return MAKE_TERM(false, SUP_TAG(sup_lab), sup1_loc);
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
// Optimized WHNF implementation with more efficient control flow
static inline Term ic_whnf(IC* ic, Term term) {
  // Cache frequently accessed values to help compiler optimization
  uint32_t stop = ic->stack_pos;
  Term next = term;
  Term* heap = ic->heap;
  Term* stack = ic->stack;
  uint32_t stack_pos = stop;
  
  // Frequently accessed locals
  TermTag tag;
  uint32_t val_loc;
  Term val;
  Term prev;
  TermTag ptag;
  
  // Branch prediction hint - most loops will continue
  while (__builtin_expect(1, 1)) {
    tag = TERM_TAG(next);
    
    // Optimized control flow using direct conditionals instead of switch
    // This helps branch prediction
    if (tag == VAR) {
      // Variable case
      val_loc = TERM_VAL(next);
      val = heap[val_loc];
      
      if (TERM_SUB(val)) {
        next = val & ~TERM_SUB_MASK; // Faster than function call
        continue;
      }
    }
    else if (IS_COL(tag)) {
      // Collapser case - both CO0 and CO1
      val_loc = TERM_VAL(next);
      val = heap[val_loc];
      
      if (TERM_SUB(val)) {
        next = val & ~TERM_SUB_MASK; // Faster than function call
        continue;
      } else {
        stack[stack_pos++] = next;
        next = val;
        continue;
      }
    }
    else if (tag == APP) {
      // Application case
      val_loc = TERM_VAL(next);
      stack[stack_pos++] = next;
      next = heap[val_loc]; // Reduce the function part
      continue;
    }
    
    // Fast path for empty stack - term is in WHNF
    if (stack_pos == stop) {
      ic->stack_pos = stack_pos;
      return next;
    }
    
    // Interaction Dispatcher
    prev = stack[--stack_pos];
    ptag = TERM_TAG(prev);
    
    // Optimize common paths with direct conditionals
    if (ptag == APP) {
      if (tag == LAM) {
        next = ic_app_lam(ic, prev, next);
        continue;
      }
      else if (IS_SUP(tag)) {
        next = ic_app_sup(ic, prev, next);
        continue;
      }
    }
    else if (IS_COL(ptag)) {
      if (tag == LAM) {
        next = ic_col_lam(ic, prev, next);
        continue;
      }
      else if (IS_SUP(tag)) {
        next = ic_col_sup(ic, prev, next);
        continue;
      }
    }
    
    // No interaction found, push term back to stack
    stack[stack_pos++] = prev;
    
    // Check if we're done
    if (stack_pos == stop) {
      ic->stack_pos = stack_pos;
      return next;
    }
    
    // Final stack traversal - update heap
    while (stack_pos > stop) {
      prev = stack[--stack_pos];
      ptag = TERM_TAG(prev);
      val_loc = TERM_VAL(prev);
      
      // Fast check for APP or COL terms
      if (ptag == APP || IS_COL(ptag)) {
        heap[val_loc] = next;
      }
      next = prev;
    }
    
    ic->stack_pos = stack_pos;
    return next;
  }
}


// Reduce a term to full normal form by recursively applying WHNF
// to all subterms.
// 
// @param ic The IC context
// @param term The term to normalize
// @return The fully normalized term
// Optimized version of ic_normal with better branch prediction and memory access
static inline Term ic_normal(IC* ic, Term term) {
  // Reset stack
  ic->stack_pos = 0;
  Term* heap = ic->heap;
  Term* stack = ic->stack;
  uint32_t stack_pos = 0;
  
  // Cache frequently used values
  uint32_t loc, val;
  Term current;
  TermTag tag;

  // Fast heap allocation without function call overhead
  uint32_t root_loc = ic->heap_pos++;
  heap[root_loc] = term;

  // Push initial location (fast term creation)
  stack[stack_pos++] = MAKE_TERM(false, VAR, root_loc);

  // Likely to run many iterations
  while (__builtin_expect(stack_pos > 0, 1)) {
    // Pop current location from stack
    loc = TERM_VAL(stack[--stack_pos]);

    // Get term at this location 
    current = heap[loc];

    // Reduce to WHNF (pass current stack position)
    ic->stack_pos = stack_pos;
    current = ic_whnf(ic, current);
    stack_pos = ic->stack_pos;

    // Store the WHNF term back to the heap
    heap[loc] = current;

    // Get term details
    tag = TERM_TAG(current);
    val = TERM_VAL(current);

    // Fast term creation for pushed terms
    if (tag == LAM) {
      // Lambda only has body subterm
      stack[stack_pos++] = MAKE_TERM(false, VAR, val);
    }
    else if (tag == APP) {
      // Fast check for APP (most common case in the benchmark)
      stack[stack_pos++] = MAKE_TERM(false, VAR, val);
      stack[stack_pos++] = MAKE_TERM(false, VAR, val + 1);
    }
    else if (IS_SUP(tag)) {
      // Separate path for SUP to improve branch prediction
      stack[stack_pos++] = MAKE_TERM(false, VAR, val);
      stack[stack_pos++] = MAKE_TERM(false, VAR, val + 1);
    }
    // Other term types (VAR, CO0, CO1) have no subterms
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
