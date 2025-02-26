#ifndef TYPES_H
#define TYPES_H

#include <stdint.h>
#include <stdbool.h>

// Term tags
typedef enum {
  VAR, // Variable
  LET, // Let binding
  SUP, // Superposition
  CO0, // Collapser first variable
  CO1, // Collapser second variable
  SET, // Universe type
  EMP, // Empty type
  EFQ, // Empty type elimination
  UNI, // Unit type
  NIL, // Unit value
  USE, // Unit elimination
  BIT, // Bool type
  BT0, // False value
  BT1, // True value
  ITE, // Bool elimination
  SIG, // Sigma type
  TUP, // Tuple (pair)
  GET, // Sigma elimination
  ALL, // Pi type
  LAM, // Lambda
  APP, // Application
  EQL, // Equality type
  RFL, // Reflexivity
  RWT  // Equality elimination
} TermTag;

// Term 32-bit packed representation
typedef uint32_t Term;

// Term components
#define TERM_SUB_MASK  0x80000000 // 1-bit: Is this a substitution?
#define TERM_TAG_MASK  0x7C000000 // 5-bits: Term tag
#define TERM_LAB_MASK  0x03000000 // 2-bits: Label for superpositions
#define TERM_VAL_MASK  0x00FFFFFF // 24-bits: Value/pointer

// Term manipulation macros
#define TERM_SUB(term)  (((term) & TERM_SUB_MASK) != 0)
#define TERM_TAG(term)  (((term) & TERM_TAG_MASK) >> 26)
#define TERM_LAB(term)  (((term) & TERM_LAB_MASK) >> 24)
#define TERM_VAL(term)  ((term) & TERM_VAL_MASK)

// Term creation macro
#define MAKE_TERM(sub, tag, lab, val) \
  (((sub) ? TERM_SUB_MASK : 0) | \
   (((tag) << 26) & TERM_TAG_MASK) | \
   (((lab) << 24) & TERM_LAB_MASK) | \
   ((val) & TERM_VAL_MASK))

// Max heap size (2^24 terms)
#define HEAP_SIZE (1 << 24)

// Interaction counter
extern uint64_t interaction_count;

#endif // TYPES_H