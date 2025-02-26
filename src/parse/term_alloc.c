#include <stddef.h>
#include "../parse.h"
#include "../memory.h"

// Helper function to allocate memory and parse a term
uint32_t parse_term_alloc(Parser* parser) {
  uint32_t loc = alloc(1);
  parse_term(parser, loc);
  return loc;
}