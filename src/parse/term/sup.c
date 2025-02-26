#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a superposition
void parse_term_sup(Parser* parser, uint32_t loc) {
  expect(parser, "&", "for superposition");
  
  uint8_t label = parse_uint(parser) & 3; // Ensure it fits in 2 bits
  expect(parser, "{", "after label in superposition");
  
  uint32_t sup_node = alloc_term(2);
  uint32_t lft_loc = sup_node;
  uint32_t rgt_loc = sup_node + 1;
  
  parse_term(parser, lft_loc);
  expect(parser, ",", "between terms in superposition");
  
  parse_term(parser, rgt_loc);
  expect(parser, "}", "after terms in superposition");
  
  store_term(loc, SUP, label, sup_node);
}