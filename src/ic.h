//./../IC.md//
//./ic.c//

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
  VAR = 0x0, // Variable
  LAM = 0x1, // Lambda
  APP = 0x2, // Application
  ERA = 0x3, // Erasure (not used yet)
  SP0 = 0x4, // Superposition with label 0
  SP1 = 0x5, // Superposition with label 1
  SP2 = 0x6, // Superposition with label 2
  SP3 = 0x7, // Superposition with label 3
  DX0 = 0x8, // Duplication variable 0 with label 0
  DX1 = 0x9, // Duplication variable 0 with label 1
  DX2 = 0xA, // Duplication variable 0 with label 2
  DX3 = 0xB, // Duplication variable 0 with label 3
  DY0 = 0xC, // Duplication variable 1 with label 0
  DY1 = 0xD, // Duplication variable 1 with label 1
  DY2 = 0xE, // Duplication variable 1 with label 2
  DY3 = 0xF, // Duplication variable 1 with label 3
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
#define IS_DP0(tag) ((tag) >= DX0 && (tag) <= DX3)
#define IS_DP1(tag) ((tag) >= DY0 && (tag) <= DY3)
#define IS_DUP(tag) ((tag) >= DX0 && (tag) <= DY3)
#define SUP_TAG(lab) ((TermTag)(SP0 + ((lab) & 0x3)))
#define DP0_TAG(lab) ((TermTag)(DX0 + ((lab) & 0x3)))
#define DP1_TAG(lab) ((TermTag)(DY0 + ((lab) & 0x3)))

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

// -----------------------------------------------------------------------------
// IC Functions
// -----------------------------------------------------------------------------

// Function declarations
IC* ic_new(uint32_t heap_size, uint32_t stack_size);
IC* ic_default_new();
void ic_free(IC* ic);
uint32_t ic_alloc(IC* ic, uint32_t n);
Term ic_make_term(TermTag tag, uint32_t val);
Term ic_make_sub(Term term);
Term ic_clear_sub(Term term);
Term ic_make_sup(uint8_t lab, uint32_t val);
Term ic_make_co0(uint8_t lab, uint32_t val);
Term ic_make_co1(uint8_t lab, uint32_t val);
uint32_t ic_lam(IC* ic, Term bod);
uint32_t ic_app(IC* ic, Term fun, Term arg);
uint32_t ic_sup(IC* ic, Term lft, Term rgt);
uint32_t ic_dup(IC* ic, Term val);
Term ic_app_lam(IC* ic, Term app, Term lam);
Term ic_app_sup(IC* ic, Term app, Term sup);
Term ic_dup_lam(IC* ic, Term dup, Term lam);
Term ic_dup_sup(IC* ic, Term dup, Term sup);
Term ic_whnf(IC* ic, Term term);
Term ic_normal(IC* ic, Term term);

#endif // IC_H

