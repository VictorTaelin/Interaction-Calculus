#include <stddef.h>
#include "../../parse.h"
#include "../../memory.h"

// Parse a superposition
void parse_term_sup(Parser* parser, uint64_t loc) {
  expect(parser, "&", "for superposition");

  uint8_t label = parse_uint(parser) & 3; // Ensure it fits in 2 bits
  expect(parser, "{", "after label in superposition");

  uint64_t sup_node = alloc(2);
  uint64_t lft_loc = sup_node;
  uint64_t rgt_loc = sup_node + 1;

  parse_term(parser, lft_loc);
  expect(parser, ",", "between terms in superposition");

  parse_term(parser, rgt_loc);
  expect(parser, "}", "after terms in superposition");

  store_term(loc, SUP, label, sup_node);
}
