#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Store a parsed term in a given location
void store_term(uint32_t loc, TermTag tag, uint8_t label, uint32_t value) {
  heap[loc] = make_term(tag, label, value);
}