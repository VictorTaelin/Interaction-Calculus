#ifndef TYPES_H
#define TYPES_H

#include <stdint.h>
#include <stdbool.h>

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
#define TERM_SUB_MASK  0x80000000UL // 1-bit: Is this a substitution?
#define TERM_TAG_MASK  0x70000000UL // 3-bits: Term tag
#define TERM_LAB_MASK  0x0C000000UL // 2-bits: Label for superpositions
#define TERM_VAL_MASK  0x03FFFFFFUL // 26-bits: Value/pointer

// Term manipulation macros
#define TERM_SUB(term)  (((term) & TERM_SUB_MASK) != 0)
#define TERM_TAG(term)  (((term) & TERM_TAG_MASK) >> 28)
#define TERM_LAB(term)  (((term) & TERM_LAB_MASK) >> 26)
#define TERM_VAL(term)  ((term) & TERM_VAL_MASK)

// Term creation macro
#define MAKE_TERM(sub, tag, lab, val) \
  (((sub) ? TERM_SUB_MASK : 0) | \
   (((uint32_t)(tag) << 28) & TERM_TAG_MASK) | \
   (((uint32_t)(lab) << 26) & TERM_LAB_MASK) | \
   ((uint32_t)(val) & TERM_VAL_MASK))

// Interaction counter
extern uint64_t interaction_count;

#endif // TYPES_H
