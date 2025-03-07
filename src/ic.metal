#include <metal_stdlib>
#include <metal_atomic>
using namespace metal;

/**
 * Interaction Calculus (IC) - Metal implementation
 * 
 * This file contains the Metal GPU implementation of the Interaction Calculus:
 * - Term representation and bit manipulation
 * - Core interactions (app_lam, app_sup, col_lam, col_sup)
 * - Weak Head Normal Form (WHNF) reduction
 * - Full Normal Form reduction
 */

// Core Term representation and constants (aligned with ic.h)
typedef uint32_t Term;

// Term tags (matching the enum in ic.h)
constant uint VAR = 0;
constant uint SUP = 1; 
constant uint CO0 = 2;
constant uint CO1 = 3;
constant uint LAM = 4;
constant uint APP = 5;

// Term components
constant uint32_t TERM_SUB_MASK = 0x80000000;
constant uint32_t TERM_TAG_MASK = 0x70000000;
constant uint32_t TERM_LAB_MASK = 0x0C000000;
constant uint32_t TERM_VAL_MASK = 0x03FFFFFF;

// -----------------------------------------------------------------------------
// Term Manipulation Macros
// -----------------------------------------------------------------------------

#define M_IC_MAKE_SUB(term) ((term) | TERM_SUB_MASK)
#define M_IC_CLEAR_SUB(term) ((term) & ~TERM_SUB_MASK)
#define M_IC_GET_TAG(term) (((term) & TERM_TAG_MASK) >> 28)
#define M_IC_GET_LAB(term) (((term) & TERM_LAB_MASK) >> 26)
#define M_IC_GET_VAL(term) ((term) & TERM_VAL_MASK)
#define M_IC_MAKE_TERM(tag, lab, val) \
  (((uint32_t)(tag) << 28) | ((uint32_t)(lab) << 26) | ((uint32_t)(val) & TERM_VAL_MASK))

// Key constants for faster case switching
#define INTERACTION_APP_LAM ((APP << 3) | LAM)
#define INTERACTION_APP_SUP ((APP << 3) | SUP)

// -----------------------------------------------------------------------------
// Memory Management Functions
// -----------------------------------------------------------------------------

/**
 * Allocate n consecutive terms in memory.
 * @param heap_pos Current heap position reference
 * @param n Number of terms to allocate
 * @param heap_size Total size of the heap
 * @return Location in the heap
 */
inline uint32_t m_ic_alloc(device uint32_t& heap_pos, uint32_t n, 
                         constant uint32_t& heap_size) {
  uint32_t ptr = heap_pos;
  heap_pos += n;
  
  // Bounds check
  if (heap_pos >= heap_size) {
    // Cap at maximum size as a safeguard
    heap_pos = heap_size - 1;
  }
  
  return ptr;
}

// -----------------------------------------------------------------------------
// Interaction Functions
// -----------------------------------------------------------------------------

/**
 * Apply a lambda to an argument.
 * @param heap Heap memory
 * @param interactions Interaction counter
 * @param app Application term
 * @param lam Lambda term
 * @return Result of the interaction
 */
inline Term m_ic_app_lam(device Term* heap, device atomic_uint& interactions,
                       Term app, Term lam) {
  atomic_fetch_add_explicit(&interactions, 1, memory_order_relaxed);
  
  // Extract locations
  const uint32_t app_loc = M_IC_GET_VAL(app);
  const uint32_t lam_loc = M_IC_GET_VAL(lam);
  
  // Load arguments
  const Term arg = heap[app_loc + 1];
  const Term bod = heap[lam_loc + 0];
  
  // Create substitution for the lambda variable 
  heap[lam_loc] = M_IC_MAKE_SUB(arg);
  
  return bod;
}

/**
 * Apply a function to a superposition.
 * @param heap Heap memory
 * @param interactions Interaction counter
 * @param heap_pos Current heap position
 * @param heap_size Total heap size
 * @param app Application term
 * @param sup Superposition term
 * @return Result of the interaction
 */
inline Term m_ic_app_sup(device Term* heap, device atomic_uint& interactions,
                       device uint32_t& heap_pos, constant uint32_t& heap_size,
                       Term app, Term sup) {
  atomic_fetch_add_explicit(&interactions, 1, memory_order_relaxed);
  
  // Cache frequent values
  const uint32_t app_loc = M_IC_GET_VAL(app);
  const uint32_t sup_loc = M_IC_GET_VAL(sup);
  const uint8_t sup_lab = M_IC_GET_LAB(sup);
  
  // Load arguments
  const Term arg = heap[app_loc + 1];
  const Term rgt = heap[sup_loc + 1];
  
  // Allocate memory
  const uint32_t col_loc = m_ic_alloc(heap_pos, 1, heap_size);
  const uint32_t app1_loc = m_ic_alloc(heap_pos, 2, heap_size);
  
  // Store arg in collapser location
  heap[col_loc] = arg;
  
  // Create CO0 and CO1 terms
  const Term x0 = M_IC_MAKE_TERM(CO0, sup_lab, col_loc);
  const Term x1 = M_IC_MAKE_TERM(CO1, sup_lab, col_loc);
  
  // Reuse sup_loc for app0 (lft is already in heap[sup_loc + 0])
  heap[sup_loc + 1] = x0;
  
  // Set up app1
  heap[app1_loc + 0] = rgt;
  heap[app1_loc + 1] = x1;
  
  // Reuse app_loc for result superposition
  heap[app_loc + 0] = M_IC_MAKE_TERM(APP, 0, sup_loc);
  heap[app_loc + 1] = M_IC_MAKE_TERM(APP, 0, app1_loc);
  
  // Return the final result
  return M_IC_MAKE_TERM(SUP, sup_lab, app_loc);
}

/**
 * Collapse a lambda.
 * @param heap Heap memory
 * @param interactions Interaction counter
 * @param heap_pos Current heap position
 * @param heap_size Total heap size
 * @param col Duplication term
 * @param lam Lambda term
 * @return Result of the interaction
 */
inline Term m_ic_col_lam(device Term* heap, device atomic_uint& interactions,
                        device uint32_t& heap_pos, constant uint32_t& heap_size,
                        Term col, Term lam) {
  atomic_fetch_add_explicit(&interactions, 1, memory_order_relaxed);
  
  // Cache frequent values
  const uint32_t col_loc = M_IC_GET_VAL(col);
  const uint32_t lam_loc = M_IC_GET_VAL(lam);
  const uint8_t col_lab = M_IC_GET_LAB(col);
  const uint8_t is_co0 = (M_IC_GET_TAG(col) == CO0);
  
  // Load body
  const Term bod = heap[lam_loc + 0];
  
  // Batch allocate memory for efficiency
  const uint32_t alloc_start = m_ic_alloc(heap_pos, 5, heap_size);
  const uint32_t lam0_loc = alloc_start;
  const uint32_t lam1_loc = alloc_start + 1;
  const uint32_t sup_loc = alloc_start + 2; // 2 locations
  const uint32_t col_new_loc = alloc_start + 4;
  
  // Set up superposition
  heap[sup_loc + 0] = M_IC_MAKE_TERM(VAR, 0, lam0_loc);
  heap[sup_loc + 1] = M_IC_MAKE_TERM(VAR, 0, lam1_loc);
  
  // Replace lambda's variable with the superposition
  heap[lam_loc] = M_IC_MAKE_SUB(M_IC_MAKE_TERM(SUP, col_lab, sup_loc));
  
  // Set up the new collapser
  heap[col_new_loc] = bod;
  
  // Set up new lambda bodies
  heap[lam0_loc] = M_IC_MAKE_TERM(CO0, col_lab, col_new_loc);
  heap[lam1_loc] = M_IC_MAKE_TERM(CO1, col_lab, col_new_loc);
  
  // Create and return the appropriate lambda
  if (is_co0) {
    heap[col_loc] = M_IC_MAKE_SUB(M_IC_MAKE_TERM(LAM, 0, lam1_loc));
    return M_IC_MAKE_TERM(LAM, 0, lam0_loc);
  } else {
    heap[col_loc] = M_IC_MAKE_SUB(M_IC_MAKE_TERM(LAM, 0, lam0_loc));
    return M_IC_MAKE_TERM(LAM, 0, lam1_loc);
  }
}

/**
 * Collapse a superposition.
 * @param heap Heap memory
 * @param interactions Interaction counter
 * @param heap_pos Current heap position
 * @param heap_size Total heap size
 * @param col Duplication term
 * @param sup Superposition term
 * @return Result of the interaction
 */
inline Term m_ic_col_sup(device Term* heap, device atomic_uint& interactions,
                        device uint32_t& heap_pos, constant uint32_t& heap_size,
                        Term col, Term sup) {
  atomic_fetch_add_explicit(&interactions, 1, memory_order_relaxed);
  
  // Cache frequent values
  const uint32_t col_loc = M_IC_GET_VAL(col);
  const uint32_t sup_loc = M_IC_GET_VAL(sup);
  const uint8_t col_lab = M_IC_GET_LAB(col);
  const uint8_t sup_lab = M_IC_GET_LAB(sup);
  const uint8_t is_co0 = (M_IC_GET_TAG(col) == CO0);
  
  // Load values needed for both paths
  const Term lft = heap[sup_loc + 0];
  const Term rgt = heap[sup_loc + 1];
  
  // Fast path for matching labels (common case)
  if (col_lab == sup_lab) {
    // Labels match: simple substitution
    if (is_co0) {
      heap[col_loc] = M_IC_MAKE_SUB(rgt);
      return lft;
    } else {
      heap[col_loc] = M_IC_MAKE_SUB(lft);
      return rgt;
    }
  } else {
    // Labels don't match: create nested collapsers
    const uint32_t sup_start = m_ic_alloc(heap_pos, 4, heap_size); // 2 sups with 2 terms each
    const uint32_t sup0_loc = sup_start;
    const uint32_t sup1_loc = sup_start + 2;
    
    // Use existing locations as collapser locations
    const uint32_t col_lft_loc = sup_loc + 0;
    const uint32_t col_rgt_loc = sup_loc + 1;
    
    // Set up the first superposition (for CO0)
    heap[sup0_loc + 0] = M_IC_MAKE_TERM(CO0, col_lab, col_lft_loc);
    heap[sup0_loc + 1] = M_IC_MAKE_TERM(CO0, col_lab, col_rgt_loc);
    
    // Set up the second superposition (for CO1)
    heap[sup1_loc + 0] = M_IC_MAKE_TERM(CO1, col_lab, col_lft_loc);
    heap[sup1_loc + 1] = M_IC_MAKE_TERM(CO1, col_lab, col_rgt_loc);
    
    // Set up original collapsers to point to lft and rgt
    heap[col_lft_loc] = lft;
    heap[col_rgt_loc] = rgt;
    
    if (is_co0) {
      heap[col_loc] = M_IC_MAKE_SUB(M_IC_MAKE_TERM(SUP, sup_lab, sup1_loc));
      return M_IC_MAKE_TERM(SUP, sup_lab, sup0_loc);
    } else {
      heap[col_loc] = M_IC_MAKE_SUB(M_IC_MAKE_TERM(SUP, sup_lab, sup0_loc));
      return M_IC_MAKE_TERM(SUP, sup_lab, sup1_loc);
    }
  }
}

// -----------------------------------------------------------------------------
// Term Normalization
// -----------------------------------------------------------------------------

/**
 * Reduce a term to weak head normal form (WHNF).
 * @param heap Heap memory
 * @param stack Evaluation stack
 * @param stack_pos Current stack position reference
 * @param stack_size Total stack size
 * @param heap_pos Current heap position reference
 * @param heap_size Total heap size
 * @param interactions Interaction counter
 * @param term The term to reduce
 * @return The term in WHNF
 */
inline Term m_ic_whnf(device Term* heap, device Term* stack,
                    device uint32_t& stack_pos, constant uint32_t& stack_size,
                    device uint32_t& heap_pos, constant uint32_t& heap_size,
                    device atomic_uint& interactions, Term term) {
  // Cache frequently used variables in registers
  uint32_t stop = stack_pos;
  Term next = term;
  uint32_t sp = stop;
  
  // Main normalization loop
  while (true) {
    // Get tag with optimized macro
    const uint tag = M_IC_GET_TAG(next);
    
    switch (tag) {
      case VAR: {
        // Variable case
        const uint32_t var_loc = M_IC_GET_VAL(next);
        const Term subst = heap[var_loc];
        if (subst & TERM_SUB_MASK) { // Direct bit test
          next = M_IC_CLEAR_SUB(subst);
          continue;
        }
        break; // No substitution, so it's in WHNF
      }
      
      case CO0:
      case CO1: {
        // Duplication case
        const uint32_t col_loc = M_IC_GET_VAL(next);
        const Term val = heap[col_loc];
        if (val & TERM_SUB_MASK) { // Direct bit test
          next = M_IC_CLEAR_SUB(val);
          continue;
        } else {
          // Push to stack
          if (sp < stack_size) {
            stack[sp++] = next;
            next = val;
            continue;
          } else {
            // Stack overflow
            break;
          }
        }
      }
      
      case APP: {
        // Application case
        const uint32_t app_loc = M_IC_GET_VAL(next);
        
        // Push to stack
        if (sp < stack_size) {
          stack[sp++] = next;
          next = heap[app_loc]; // Reduce the function part
          continue;
        } else {
          // Stack overflow
          break;
        }
      }
      
      default: { // SUP, LAM
        // Handle default case (SUP, LAM)
        if (sp == stop) {
          stack_pos = sp; // Update stack position before return
          return next; // Stack empty, term is in WHNF
        } else {
          // Pop from stack
          Term prev = stack[--sp];
          
          // Get tag
          const uint ptag = M_IC_GET_TAG(prev);
          
          // Handle interactions based on term types
          if (ptag == APP && tag == LAM) {
            next = m_ic_app_lam(heap, interactions, prev, next);
            continue;
          } 
          else if (ptag == APP && tag == SUP) {
            next = m_ic_app_sup(heap, interactions, heap_pos, heap_size, prev, next); 
            continue;
          }
          else if ((ptag == CO0 || ptag == CO1) && tag == LAM) {
            next = m_ic_col_lam(heap, interactions, heap_pos, heap_size, prev, next);
            continue;
          }
          else if ((ptag == CO0 || ptag == CO1) && tag == SUP) {
            next = m_ic_col_sup(heap, interactions, heap_pos, heap_size, prev, next);
            continue;
          }
          
          // No interaction found, return to stack
          stack[sp++] = prev;
          break;
        }
      }
    }
    
    // After processing, check stack and update heap if needed
    if (sp == stop) {
      stack_pos = sp;
      return next; // Stack empty, return WHNF
    } else {
      // Process remaining stack
      while (sp > stop) {
        // Direct stack access
        Term host = stack[--sp];
        
        // Extract components
        const uint htag = M_IC_GET_TAG(host);
        const uint32_t hloc = M_IC_GET_VAL(host);
        
        // Update the heap with the reduced term - only for specific tags
        if (htag == APP || htag == CO0 || htag == CO1) {
          heap[hloc] = next;
        }
        next = host;
      }
      stack_pos = sp;
      return next; // Return updated original term
    }
  }
}

/**
 * Reduce a term to full normal form by recursively applying WHNF
 * to all subterms.
 * 
 * @param heap Heap memory
 * @param stack Evaluation stack
 * @param stack_pos Current stack position reference
 * @param stack_size Total stack size
 * @param heap_pos Current heap position reference
 * @param heap_size Total heap size
 * @param interactions Interaction counter
 * @param term The term to normalize
 * @return The fully normalized term
 */
inline Term m_ic_normal(device Term* heap, device Term* stack,
                      device uint32_t& stack_pos, constant uint32_t& stack_size,
                      device uint32_t& heap_pos, constant uint32_t& heap_size,
                      device atomic_uint& interactions, Term term) {
  // Reset stack
  stack_pos = 0;
  uint32_t sp = 0;
  
  // Allocate a new node for the initial term
  uint32_t root_loc = m_ic_alloc(heap_pos, 1, heap_size);
  heap[root_loc] = term;
  
  // Push initial location to stack
  stack[sp++] = (root_loc & TERM_VAL_MASK);
  
  // Main normalization loop
  while (sp > 0) {
    // Pop current location from stack
    const uint32_t loc = stack[--sp] & TERM_VAL_MASK;
    
    // Get term at this location
    Term current = heap[loc];
    
    // Reduce to WHNF
    stack_pos = sp;
    current = m_ic_whnf(heap, stack, stack_pos, stack_size,
                       heap_pos, heap_size, interactions, current);
    sp = stack_pos;
    
    // Store the WHNF term back to the heap
    heap[loc] = current;
    
    // Get term details
    const uint tag = M_IC_GET_TAG(current);
    const uint32_t val = M_IC_GET_VAL(current);
    
    // Push subterm locations based on term type
    if (tag == LAM) {
      if (sp < stack_size) {
        stack[sp++] = val & TERM_VAL_MASK;
      }
    }
    else if (tag == APP || tag == SUP) {
      // Both APP and SUP need to push two locations
      if (sp + 1 < stack_size) {
        stack[sp++] = val & TERM_VAL_MASK;
        stack[sp++] = (val + 1) & TERM_VAL_MASK;
      }
    }
    // Other tags have no subterms to process
  }
  
  // Update stack position and return the fully normalized term
  stack_pos = sp;
  return heap[root_loc];
}

/**
 * Main Metal kernel function to normalize a term.
 */
kernel void normalizeKernel(device Term* heap [[buffer(0)]],
                          device Term* stack [[buffer(1)]],
                          device uint32_t& heap_pos [[buffer(2)]],
                          device uint32_t& stack_pos [[buffer(3)]],
                          device atomic_uint& interactions [[buffer(4)]],
                          constant uint32_t& heap_size [[buffer(5)]],
                          constant uint32_t& stack_size [[buffer(6)]],
                          uint gid [[thread_position_in_grid]]) {
  // Only use thread 0 in the grid
  if (gid == 0) {
    // Get the term from the heap's entry point
    Term term = heap[0];
    
    // Perform normalization
    term = m_ic_normal(heap, stack, stack_pos, stack_size, 
                     heap_pos, heap_size, interactions, term);
    
    // Store the result back to the heap's entry point
    heap[0] = term;
  }
}