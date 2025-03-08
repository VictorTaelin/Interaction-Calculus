#ifndef IC_H
#define IC_H

// -----------------------------------------------------------------------------
// Interaction Calculus (IC) - Core header-only implementation
// 
// This file contains the full implementation of the Interaction Calculus:
// - Term representation and bit manipulation
// - Memory management
// - Core interactions (app_lam, app_sup, dup_lam, dup_sup)
// - Weak Head Normal Form (WHNF) reduction
// - Full Normal Form reduction
// -----------------------------------------------------------------------------

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Default heap and stack sizes
#define IC_DEFAULT_HEAP_SIZE (1 << 27) // 128M terms
#define IC_DEFAULT_STACK_SIZE (1 << 24) // 16M terms

#ifdef HAVE_METAL
#include <stdbool.h>
// Forward declarations for Metal functions
bool metal_is_available();
void metal_execute_sup_reduction(uint32_t* heap, uint32_t sup_count, uint32_t* sup_indices);
#endif

// -----------------------------------------------------------------------------
// Core Types and Constants
// -----------------------------------------------------------------------------

// Term tags for new, compact memory format
typedef enum {
  VAR = 0x00, // Variable
  LAM = 0x01, // Lambda
  APP = 0x02, // Application
  ERA = 0x03, // Erasure
  NUM = 0x04, // Number
  SUC = 0x05, // Successor
  SWI = 0x06, // Switch
  TMP = 0x07, // Temporary
  SP0 = 0x08, // Superposition with label 0
  SP1 = 0x09, // Superposition with label 1
  SP2 = 0x0A, // Superposition with label 2
  SP3 = 0x0B, // Superposition with label 3
  SP4 = 0x0C, // Superposition with label 4
  SP5 = 0x0D, // Superposition with label 5
  SP6 = 0x0E, // Superposition with label 6
  SP7 = 0x0F, // Superposition with label 7
  DX0 = 0x10, // Duplication variable 0 with label 0
  DX1 = 0x11, // Duplication variable 0 with label 1
  DX2 = 0x12, // Duplication variable 0 with label 2
  DX3 = 0x13, // Duplication variable 0 with label 3
  DX4 = 0x14, // Duplication variable 0 with label 4
  DX5 = 0x15, // Duplication variable 0 with label 5
  DX6 = 0x16, // Duplication variable 0 with label 6
  DX7 = 0x17, // Duplication variable 0 with label 7
  DY0 = 0x18, // Duplication variable 1 with label 0
  DY1 = 0x19, // Duplication variable 1 with label 1
  DY2 = 0x1A, // Duplication variable 1 with label 2
  DY3 = 0x1B, // Duplication variable 1 with label 3
  DY4 = 0x1C, // Duplication variable 1 with label 4
  DY5 = 0x1D, // Duplication variable 1 with label 5
  DY6 = 0x1E, // Duplication variable 1 with label 6
  DY7 = 0x1F, // Duplication variable 1 with label 7
} TermTag;

// Term 32-bit packed representation
typedef uint32_t Term;

// Term components
#define TERM_SUB_MASK 0x80000000UL // 1-bit: Is this a substitution?
#define TERM_TAG_MASK 0x7C000000UL // 5-bits: Term tag
#define TERM_VAL_MASK 0x03FFFFFFUL // 26-bits: Value/pointer

// Term component extraction
#define TERM_SUB(term) (((term) & TERM_SUB_MASK) != 0)
#define TERM_TAG(term) ((TermTag)(((term) & TERM_TAG_MASK) >> 26))
#define TERM_VAL(term) ((term) & TERM_VAL_MASK)

// Label helpers (for compatibility with existing code)
#define TERM_LAB(term) ((TERM_TAG(term) & 0x7)) // Extract label from tag (last 3 bits)
#define IS_SUP(tag) ((tag) >= SP0 && (tag) <= SP7)
#define IS_DP0(tag) ((tag) >= DX0 && (tag) <= DX7)
#define IS_DP1(tag) ((tag) >= DY0 && (tag) <= DY7)
#define IS_DUP(tag) ((tag) >= DX0 && (tag) <= DY7)
#define IS_ERA(tag) ((tag) == ERA)
#define IS_NUM(tag) ((tag) == NUM)
#define IS_SUC(tag) ((tag) == SUC)
#define IS_SWI(tag) ((tag) == SWI)
#define SUP_TAG(lab) ((TermTag)(SP0 + ((lab) & 0x7)))
#define DP0_TAG(lab) ((TermTag)(DX0 + ((lab) & 0x7)))
#define DP1_TAG(lab) ((TermTag)(DY0 + ((lab) & 0x7)))

// Term creation
#define MAKE_TERM(sub, tag, val) \
  (((sub) ? TERM_SUB_MASK : 0) | \
   (((uint32_t)(tag) << 26)) | \
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

// -----------------------------------------------------------------------------
// IC Functions
// -----------------------------------------------------------------------------

// Create a new IC context with the specified heap and stack sizes.  
// @param heap_size Number of terms in the heap  
// @param stack_size Number of terms in the stack  
// @return A new IC context or NULL if allocation failed  
IC* ic_new(uint32_t heap_size, uint32_t stack_size);  

// Create a new IC context with default heap and stack sizes.  
// @return A new IC context or NULL if allocation failed  
IC* ic_default_new();  

// Free all resources associated with an IC context.  
// @param ic The IC context to free  
void ic_free(IC* ic);  

// Allocate n consecutive terms in the heap.  
// @param ic The IC context  
// @param n Number of terms to allocate  
// @return The starting location of the allocated block  
uint32_t ic_alloc(IC* ic, uint32_t n);  

// Create a term with the given tag and value.  
// @param tag The term's tag  
// @param val The term's value (typically a heap location)  
// @return The constructed term  
Term ic_make_term(TermTag tag, uint32_t val);  

// Create a substitution term by setting the substitution bit.  
// @param term The term to convert to a substitution  
// @return The term with its substitution bit set  
Term ic_make_sub(Term term);  

// Clear the substitution bit from a term.  
// @param term The term to clear  
// @return The term with its substitution bit cleared  
Term ic_clear_sub(Term term);  

// Term constructors.
Term ic_make_sup(uint8_t lab, uint32_t val);  
Term ic_make_co0(uint8_t lab, uint32_t val);  
Term ic_make_co1(uint8_t lab, uint32_t val);  
Term ic_make_era();
Term ic_make_num(uint32_t val);
Term ic_make_suc(uint32_t val);
Term ic_make_swi(uint32_t val);

// Check if a term is an erasure term.  
// @param term The term to check  
// @return True if the term is an erasure, false otherwise  
bool ic_is_era(Term term);  

// Allocate a node in the heap.  
uint32_t ic_lam(IC* ic, Term bod);  
uint32_t ic_app(IC* ic, Term fun, Term arg);  
uint32_t ic_sup(IC* ic, Term lft, Term rgt);  
uint32_t ic_dup(IC* ic, Term val);
uint32_t ic_suc(IC* ic, Term num);
uint32_t ic_swi(IC* ic, Term num, Term ifz, Term ifs);

// Interactions
Term ic_app_lam(IC* ic, Term app, Term lam);  
Term ic_app_sup(IC* ic, Term app, Term sup);  
Term ic_app_era(IC* ic, Term app, Term era);  
Term ic_dup_lam(IC* ic, Term dup, Term lam);  
Term ic_dup_sup(IC* ic, Term dup, Term sup);  
Term ic_dup_era(IC* ic, Term dup, Term era);

// Numeric interactions
Term ic_suc_num(IC* ic, Term suc, Term num);
Term ic_suc_era(IC* ic, Term suc, Term era);
Term ic_suc_sup(IC* ic, Term suc, Term sup);
Term ic_swi_num(IC* ic, Term swi, Term num);
Term ic_swi_era(IC* ic, Term swi, Term era);
Term ic_swi_sup(IC* ic, Term swi, Term sup);

// Reduce a term to weak head normal form (WHNF).  
// @param ic The IC context  
// @param term The term to reduce  
// @return The term in WHNF  
Term ic_whnf(IC* ic, Term term);  

// Reduce a term to full normal form by recursively normalizing subterms.  
// @param ic The IC context  
// @param term The term to normalize  
// @return The normalized term  
Term ic_normal(IC* ic, Term term);  

#define NONE 0xFFFFFFFF

#endif // IC_H
